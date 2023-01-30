(ns more-speech.nostr.event-composers
  (:require [more-speech.nostr.events :as events]
            [more-speech.mem :refer :all]
            [more-speech.nostr.util :refer :all]
            [more-speech.nostr.elliptic-signature :as ecc]
            [more-speech.nostr.util :as util]
            [more-speech.nostr.contact-list :as contact-list]
            [more-speech.config :as config :refer [get-db]]
            [clojure.string :as string]
            [clojure.core.async :as async]
            [more-speech.db.gateway :as gateway])
  (:import (ecdhJava SECP256K1)))

(defn body->event
  "Adds pubkey, created-at, id, and sig to the partially composed body,
  which must include kind, tags, and content.  The body is put into an
  EVENT wrapper that is ready to send."
  [body]
  (let [keys (get-mem :keys)
        private-key (util/hex-string->bytes (:private-key keys))
        pubkey (util/hex-string->bytes (:public-key keys))
        now (quot (System/currentTimeMillis) 1000)
        body (assoc body :pubkey (util/bytes->hex-string pubkey)
                         :created_at now)
        [id body] (events/make-id-with-pow config/proof-of-work-default body)
        ;id (make-id body)
        aux-rand (util/num->bytes 32 (biginteger (System/currentTimeMillis)))
        signature (ecc/do-sign id private-key aux-rand)
        event (assoc body :id (util/bytes->hex-string id)
                          :sig (util/bytes->hex-string signature))]
    ["EVENT" event]))

(defn compose-metadata-event []
  (let [keys (get-mem :keys)
        content (events/to-json {:name (:name keys)
                          :about (:about keys)
                          :picture (:picture keys)})
        body {:kind 0
              :tags []
              :content content}]
    (body->event body)))

(defn make-contact-list-tag [contact-entry]
  (let [petname (get contact-entry :petname "")
        petname (if (nil? petname) "" petname)]
    [:p (events/hexify (:pubkey contact-entry)) "" petname]))

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
     [[:e (events/hexify root) "" "root"] [:e (events/hexify reply-to) "" "reply"]]))

  ([reply-to]
   (if (nil? reply-to)
     []
     [[:e (events/hexify reply-to) "" "reply"]])))

(defn make-people-reference-tags [pubkey reply-to-or-nil]
  (if (nil? reply-to-or-nil)
    []
    (let [
          parent-event-id reply-to-or-nil
          parent-event (gateway/get-event (get-db) parent-event-id)
          parent-tags (:tags parent-event)
          people-ids (map second (filter #(= :p (first %)) parent-tags))
          parent-author (:pubkey parent-event)
          people-ids (conj people-ids (events/hexify parent-author))
          people-ids (remove #(= (events/hexify pubkey) %) people-ids)]
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

(defn abbreviate-pubkey [pubkey-string]
  (let [pubkey (util/hex-string->num pubkey-string)
        abbreviated-pubkey (str (subs pubkey-string 0 11) "-")]
    (gateway/add-profile (get-db) pubkey {:name abbreviated-pubkey})
    pubkey))

(defn make-emplacement [reference tags]
  (let [tags (vec tags)                                     ;conj adds to end.
        tag-index (count tags)
        user-reference (subs reference 1)
        user-id (find-user-id user-reference)
        user-id (if (and (nil? user-id)
                         (re-matches config/pubkey-pattern user-reference))
                  (abbreviate-pubkey user-reference)
                  user-id)]
    (if (nil? user-id)
      [reference tags]
      (let [tag [:p (util/num32->hex-string user-id)]
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
        pattern config/user-name-pattern
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
   (let [pubkey (get-mem :pubkey)
         root (get-reply-root reply-to-or-nil)
         tags (concat (make-event-reference-tags reply-to-or-nil root)
                      (make-people-reference-tags pubkey reply-to-or-nil)
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

(defn compose-and-send-metadata-event []
  (send-event (compose-metadata-event)))

(defn compose-and-send-contact-list [contact-list]
  (send-event (compose-contact-list contact-list)))
