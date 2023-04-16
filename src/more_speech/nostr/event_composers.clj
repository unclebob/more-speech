(ns more-speech.nostr.event-composers
  (:require [clojure.core.async :as async]
            [clojure.string :as string]
            [more-speech.bech32 :as bech32]
            [more-speech.config :as config :refer [get-db]]
            [more-speech.db.gateway :as gateway]
            [more-speech.logger.default :refer [log-pr]]
            [more-speech.mem :refer :all]
            [more-speech.nostr.contact-list :as contact-list]
            [more-speech.nostr.elliptic-signature :as ecc]
            [more-speech.nostr.events :as events]
            [more-speech.nostr.util :refer :all]
            [more-speech.nostr.util :as util])
  (:import (ecdhJava SECP256K1)))

(defn body->event
  "Adds pubkey, created-at, id, and sig to the partially composed body,
  which must include kind, tags, and content.  The body is put into an
  EVENT wrapper that is ready to send."
  [body]
  (let [private-key (util/hex-string->bytes (get-mem [:keys :private-key]))
        pubkey (get-mem :pubkey)
        now (util/get-now)
        body (assoc body :pubkey (util/hexify pubkey)
                         :created_at now)
        [id body] (events/make-id-with-pow config/proof-of-work-default body)
        aux-rand (util/num->bytes 32 (biginteger (System/currentTimeMillis)))
        signature (ecc/do-sign id private-key aux-rand)
        event (assoc body :id (util/bytes->hex-string id)
                          :sig (util/bytes->hex-string signature))]
    ["EVENT" event]))

(defn compose-metadata-event []
  (let [key-map (get-mem :keys)
        profile {:name (:name key-map)
                 :about (:about key-map)
                 :picture (:picture key-map)}
        profile (if (some? (:nip05 key-map))
                  (assoc profile :nip05 (:nip05 key-map))
                  profile)
        profile (if (some? (:lud16 key-map))
                  (assoc profile :lud16 (:lud16 key-map))
                  profile)
        content (events/to-json profile)
        body {:kind 0
              :tags []
              :content content}]
    (body->event body)))

(defn make-contact-list-tag [contact-entry]
  (let [petname (get contact-entry :petname "")
        petname (if (nil? petname) "" petname)]
    [:p (hexify (:pubkey contact-entry)) "" petname]))

(defn make-contact-list-tags [contact-list]
  (map make-contact-list-tag contact-list))

(defn compose-contact-list [contact-list]
  (let [tags (make-contact-list-tags contact-list)
        body {:kind 3
              :tags tags
              :content "more-speech contact list"}]
    (body->event body)))

(defn make-event-reference-tags
  ([reply-to root]
   (if (or (nil? root) (= root reply-to))
     (make-event-reference-tags reply-to)
     [[:e (hexify root) "" "root"] [:e (hexify reply-to) "" "reply"]]))

  ([reply-to]
   (if (nil? reply-to)
     []
     [[:e (hexify reply-to) "" "reply"]])))

(defn make-people-reference-tags [reply-to-or-nil]
  (if (nil? reply-to-or-nil)
    []
    (let [parent-event-id reply-to-or-nil
          parent-event (gateway/get-event (get-db) parent-event-id)
          parent-tags (:tags parent-event)
          people-ids (map second (filter #(= :p (first %)) parent-tags))
          parent-author (:pubkey parent-event)
          people-ids (conj people-ids (hexify parent-author))
          my-pubkey (get-mem :pubkey)
          people-ids (if (= (:pubkey parent-event) my-pubkey)
                       people-ids
                       (remove #(= (hexify my-pubkey) %) people-ids))]
      (map #(vector :p %) people-ids))))

(defn make-subject-tag [subject]
  (if (empty? (.trim subject))
    []
    [[:subject subject]]))

(defn get-reply-root [reply-to-or-nil]
  (if (nil? reply-to-or-nil)
    nil
    (let [reply-id reply-to-or-nil
          replied-to-event (gateway/get-event (get-db) reply-id)
          [root _mentions _referent] (events/get-references replied-to-event)]
      root)))

(defn find-user-id [user-name]
  (let [pet-pubkey (contact-list/get-pubkey-from-petname user-name)]
    (if (some? pet-pubkey)
      pet-pubkey
      (gateway/get-id-from-username (get-db) user-name))))

(defn translate-reference [user-reference]
  (try
    (cond
      (re-matches config/hex-key-pattern user-reference)
      (util/hex-string->num user-reference)

      (re-matches config/npub-reference user-reference)
      (bech32/address->number user-reference)

      :else
      nil
      )
    (catch Exception _e
      nil))
  )

(defn make-emplacement [reference tags]
  (let [tags (vec tags)                                     ;conj adds to end.
        tag-index (count tags)
        user-reference (subs reference 1)
        user-id (find-user-id user-reference)
        user-id (if (nil? user-id)
                  (translate-reference user-reference)
                  user-id)]
    (if (nil? user-id)
      [reference tags]
      (let [is-event? (gateway/event-exists? (get-db) user-id)
            tag [(if is-event? :e :p)
                 (util/num32->hex-string user-id)]
            emplacement (str "#[" tag-index "]")]
        [emplacement (conj tags tag)]))))

(defn make-emplacements [references tags]
  (loop [references references
         emplacements []
         tags tags]
    (if (empty? references)
      [emplacements tags]
      (let [reference (first references)
            [emplacement tags] (make-emplacement reference tags)]
        (recur (rest references)
               (conj emplacements emplacement)
               tags)))))

(defn emplace-references [content tags]
  (let [padded-content (str " " content " ")
        pattern config/user-reference-pattern
        references (re-seq pattern padded-content)
        segments (string/split padded-content pattern)
        [emplacements tags] (make-emplacements references tags)
        emplaced-text (string/trim
                        (apply str (interleave segments (conj emplacements ""))))]
    [emplaced-text tags]))

(defn encrypt-if-direct-message [content tags]
  (if (re-find #"^D \#\[\d+\]" content)
    (let [reference-digits (re-find #"\d+" content)
          reference-index (Integer/parseInt reference-digits)
          p-tag (get tags reference-index)]
      (if (nil? p-tag)
        [content 1]
        (let [recipient-key (hex-string->num (second p-tag))
              private-key (get-mem [:keys :private-key])
              sender-key (hex-string->num private-key)
              shared-secret (SECP256K1/calculateKeyAgreement sender-key recipient-key)
              encrypted-content (SECP256K1/encrypt shared-secret content)]
          [encrypted-content 4])))
    [content 1]))

(defn compose-text-event
  ([subject text]
   (compose-text-event subject text nil))

  ([subject text reply-to-or-nil]
   (let [root (get-reply-root reply-to-or-nil)
         tags (concat (make-event-reference-tags reply-to-or-nil root)
                      (make-people-reference-tags reply-to-or-nil)
                      (make-subject-tag subject)
                      [[:client (str "more-speech - " config/version)]])
         [content tags] (emplace-references text tags)
         [content kind] (encrypt-if-direct-message content tags)
         body {:kind kind
               :tags tags
               :content content}]
     (body->event body))))

(defn send-event [event]
  (let [send-chan (get-mem :send-chan)]
    (async/>!! send-chan [:event event])))

(defn compose-and-send-text-event [source-event-or-nil subject message]
  (let [reply-to-or-nil (:id source-event-or-nil)
        event (compose-text-event subject message reply-to-or-nil)]
    (send-event event)))

(defn compose-recommended-server-event [url]
  (let [body {:kind 2
              :tags []
              :content url}]
    (body->event body)))

(defn remove-arguments [url]
  (re-find config/relay-pattern url))

(defn compose-and-send-metadata-event []
  (send-event (compose-metadata-event)))

(defn compose-and-send-metadata-and-relay-recommendations []
  (send-event (compose-metadata-event))
  (let [server-urls (filter #(:write (get @relays %)) (keys @relays))
        server-urls (map remove-arguments server-urls)]
    (log-pr 1 'server-urls server-urls)
    (future
      (doseq [url server-urls]
        (Thread/sleep 5000)
        (send-event (compose-recommended-server-event url))))))

(defn compose-and-send-contact-list [contact-list]
  (send-event (compose-contact-list contact-list)))

(defn compose-reaction-event [subject-event polarity]
  (let [id (:id subject-event)
        pubkey (:pubkey subject-event)
        ep-tags (filter #(or (= :p (first %)) (= :e (first %))) (:tags subject-event))
        tags (concat ep-tags [[:e (hexify id)] [:p (hexify pubkey)]])
        body {:kind 7
              :tags tags
              :content polarity}]
    body))

(defn compose-and-send-reaction-event [subject-event polarity]
  (send-event
    (body->event
      (compose-reaction-event subject-event polarity))))
