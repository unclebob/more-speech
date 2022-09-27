(ns more-speech.nostr.events
  (:require [clojure.spec.alpha :as s]
            [clojure.data.json :as json]
            [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.nostr.util :refer [hex-string->num]]
            [more-speech.nostr.elliptic-signature :as ecc]
            [clojure.core.async :as async]
            [more-speech.nostr.util :as util]
            [more-speech.nostr.relays :as relays]
            [clojure.string :as string]
            [more-speech.nostr.contact-list :as contact-list]
            [more-speech.config :as config])
  (:import (java.nio.charset StandardCharsets)
           (ecdhJava SECP256K1)))

(s/def ::id number?)
(s/def ::pubkey number?)
(s/def ::created-at number?)
(s/def ::content string?)
(s/def ::sig number?)
(s/def ::tag (s/tuple keyword? number?))
(s/def ::tags (s/coll-of ::tag))
(s/def ::references (s/coll-of number?))
(s/def ::relay-url string?)
(s/def ::relays (s/coll-of ::relay-url))
(s/def ::event (s/keys :req-un [::id
                                ::pubkey
                                ::created-at
                                ::content
                                ::sig
                                ::tags
                                ::references]
                       :opt-un [::relays]))
(defn hexify [n]
  (util/num32->hex-string n))

(defprotocol event-handler
  (handle-text-event [handler event])
  (update-relay-panel [handler])
  )

(s/def ::chronological-text-events (s/coll-of ::id))
(s/def ::text-event-map (s/map-of :id :event))
(s/def ::name string?)
(s/def ::about string?)
(s/def ::picture string?)
(s/def ::profile (s/keys :req-un [::name ::about ::picture]))
(s/def ::profiles (s/map-of ::id ::profile))
(s/def ::public-key string?)
(s/def ::private-key string?)
(s/def ::keys (s/keys :req-un [::name ::about ::picture ::public-key ::private-key]))
(s/def ::read-event-ids (s/coll-of ::id :kind set?))
(s/def ::selected (s/coll-of ::id))
(s/def ::blocked (s/coll-of ::id))
(s/def ::tab (s/keys :req-un [::name ::selected ::blocked]))
(s/def ::tabs-list (s/coll-of ::tab))
(s/def ::selected-event ::id)
(s/def ::event-history (s/coll-of (s/tuple number? ::id)))
(s/def ::back-count number?)
(s/def ::backing-up boolean?)

(s/def ::event-context (s/keys :req-un [::chronological-text-events
                                        ::text-event-map
                                        ::profiles
                                        ::keys
                                        ::read-event-ids
                                        ::tabs-list
                                        ::selected-event
                                        ::event-history
                                        ::back-count
                                        ::backing-up]))

(defn make-event-context [event-context-map]
  (atom (merge {:chronological-text-events []
                :text-event-map {}
                :profiles {}
                :keys {}
                :read-event-ids #{}
                :tabs-list []
                :event-history []
                :back-count 0
                }
               event-context-map)))


(defn decrypt-his-dm [event]
  (let [p-tags (filter #(= :p (first %)) (:tags event))
        my-tag (filter #(= (hex-string->num (second %))
                           (get-event-state :pubkey))
                       p-tags)]
    (if (empty? my-tag)
      (assoc event :private true)
      (let [his-key (:pubkey event)
            priv-key (hex-string->num (get-in (get-event-state) [:keys :private-key]))
            shared-secret (SECP256K1/calculateKeyAgreement priv-key his-key)
            decrypted-content (SECP256K1/decrypt shared-secret (:content event))
            event (assoc event :content decrypted-content)]
        event))))

(defn decrypt-my-dm [event]
  (let [p-tags (filter #(= :p (first %)) (:tags event))
        p-tag (first p-tags)
        his-key (hex-string->num (second p-tag))
        priv-key (hex-string->num (get-in (get-event-state) [:keys :private-key]))
        shared-secret (SECP256K1/calculateKeyAgreement priv-key his-key)
        decrypted-content (SECP256K1/decrypt shared-secret (:content event))
        event (assoc event :content decrypted-content)]
    event))

(defn decrypt-dm-event [event]
  (if (= 4 (:kind event))
    (let [author-key (:pubkey event)
          event (assoc event :dm true)]
      (if (= author-key (get-event-state :pubkey))
        (decrypt-my-dm event)
        (decrypt-his-dm event)))
    event))

(defn select-event [event-state tab-index id]
  (swap! ui-context assoc :selected-tab tab-index)
  (if-not (:backing-up event-state)
    (-> event-state
        (update :read-event-ids conj id)
        (update :event-history conj [tab-index id])
        (assoc :selected-event id :back-count 0))
    (-> event-state (assoc :selected-event id :backing-up false))))

(defn to-json [o]
  (json/write-str o :escape-slash false :escape-unicode false))

(defn fix-name [name]
  (if (empty? name)
    (str "dud-" (rand-int 1000000))
    (let [fixed-name (apply str
                            (filter
                              #(re-matches config/user-name-chars (str %))
                              name))]
      (if (empty? fixed-name)
        (str "dudx-" (rand-int 100000))
        fixed-name))))

(defn add-suffix-for-duplicate [pubkey name]
  (let [profiles (get-event-state :profiles)
        others-profiles (dissoc profiles pubkey)
        profile-vals (vals others-profiles)
        dups (filter #(= name (:name %)) profile-vals)]
    (if (empty? dups)
      name
      (str name (rand-int 1000)))))

(defn process-name-event [event-state {:keys [_id pubkey _created-at _kind _tags content _sig] :as event}]
  (try
    (let [profile (json/read-str content)
          name (get profile "name" "tilt")
          about (get profile "about" "")
          picture (get profile "picture" "")
          name (add-suffix-for-duplicate pubkey (fix-name name))]
      (-> event-state
          (update-in [:profiles] assoc pubkey {:name name
                                               :about about
                                               :picture picture})))
    (catch Exception e
      (prn 'json-exception-process-name-event-ignored (.getMessage e))
      (prn event)
      event-state)))


(defn get-unmarked-references [e-tags]
  (let [refs (map second e-tags)
        refs (map hex-string->num refs)
        root (if (empty? refs) nil (first refs))
        referent (last refs)
        mentions (drop-last (rest refs))]
    [root mentions referent])
  )

(defn get-marked-references [e-tags]
  (loop [tags e-tags
         root nil
         referent nil
         mentions []]
    (if (empty? tags)
      (if (nil? root)
        [referent mentions referent]
        [root mentions referent])
      (let [tag (first tags)
            id (hex-string->num (nth tag 1))]
        (if (>= (count tag) 4)
          (condp = (nth tag 3)
            "reply" (recur (rest tags) root id mentions)
            "root" (recur (rest tags) id referent mentions)
            (recur (rest tags) root referent (conj mentions id)))
          (recur (rest tags) root referent (conj mentions id)))))))


(defn get-references
  "returns [root mentions referent] as BigIntegers.
  root is the root id of the thread.
  referent is the id of the event being replied to.
  mentions is a list of cited ids.
  (See NIP-10)"
  [event]
  (try
    (let [tags (:tags event)
          e-tags (filter #(= :e (first %)) tags)
          markers (set (map #(nth % 3) (filter #(>= (count %) 4) e-tags)))]
      (if (contains? markers "reply")
        (get-marked-references e-tags)
        (get-unmarked-references e-tags)))
    (catch Exception e
      (prn 'get-references 'bad-tags-in-event (.getMessage e) event)
      [nil nil nil])))

(defn process-references [event-state event]
  (let [[_ _ referent] (get-references event)]
    (if (nil? referent)
      event-state
      (let [referent-path [:text-event-map referent]]
        (if (nil? (get-in event-state referent-path))
          event-state
          (update-in
            event-state
            (concat referent-path [:references])
            conj (:id event)))))))

(defn add-event [event-state event urls]
  (let [id (:id event)
        time (:created-at event)]
    (if (contains? (:text-event-map event-state) id)
      (update-in event-state [:text-event-map id :relays] concat urls)
      (-> event-state
          (assoc-in [:text-event-map id] event)
          (assoc-in [:text-event-map id :relays] urls)
          (update-in [:chronological-text-events] conj [id time])
          (update :days-changed conj (quot time 86400))
          (process-references event)))))

(defn process-text-event [event-state event url]
  (let [event-state (add-event event-state event [url])]
    (relays/add-recommended-relays-in-tags event)
    event-state))

(defn process-tag [tag]
  (when (seq tag)
    (let [tag-type (first tag)
          tag-args (rest tag)
          tag-type (.replace tag-type \: \-)]
      (concat [(keyword tag-type)] tag-args))))

(defn process-tags [tags]
  (map process-tag tags))

(defn translate-event [event]
  (let [id (hex-string->num (get event "id"))
        pubkey (hex-string->num (get event "pubkey"))
        sig (hex-string->num (get event "sig"))]
    {:id id
     :pubkey pubkey
     :created-at (get event "created_at")
     :kind (get event "kind")
     :content (get event "content")
     :sig sig
     :tags (process-tags (get event "tags"))}))

(defn process-like [event-state _event]
  event-state)

(defn process-server-recommendation [event-state event]
  (relays/add-relay (:content event))
  event-state)

(defn process-event [{:keys [profiles] :as event-state} event url]
  (let [_name-of (fn [pubkey] (get-in profiles [pubkey :name] pubkey))
        {:keys [id pubkey _created-at kind _tags _content sig]} event
        valid? (ecc/do-verify (util/num->bytes 32 id)
                              (util/num->bytes 32 pubkey)
                              (util/num->bytes 64 sig))
        ]
    (if (not valid?)
      (do
        (prn 'signature-verification-failed url event)
        event-state)
      (condp = kind
        0 (process-name-event event-state event)
        1 (process-text-event event-state event url)
        2 (process-server-recommendation event-state event)
        3 (contact-list/process-contact-list event-state event url)
        4 (process-text-event event-state event url)
        7 (process-like event-state event)
        (do #_(prn "unknown event: " url event)
          event-state)))))

(defn get-root-of-thread [id]
  (let [messages (get-event-state :text-event-map)
        event (get messages id)
        [root _ _] (get-references event)]
    (if (some? root) root id)))

(defn chronological-event-comparator [[i1 t1] [i2 t2]]
  (if (= i1 i2)
    0
    (compare t2 t1)))

(defn make-chronological-text-events []
  (sorted-set-by chronological-event-comparator))

(defn make-id
  "returns byte array of id given the clojure form of the body"
  [{:keys [pubkey created_at kind tags content]}]
  (let [id-event (to-json [0 pubkey created_at kind tags content])
        id (util/sha-256 (.getBytes id-event StandardCharsets/UTF_8))]
    id)
  )

(defn body->event
  "Adds pubkey, created-at, id, and sig to the partially composed body,
  which must include kind, tags, and content.  The body is put into an
  EVENT wrapper that is ready to send."
  [body]
  (let [keys (get-event-state :keys)
        private-key (util/hex-string->bytes (:private-key keys))
        pubkey (util/hex-string->bytes (:public-key keys))
        now (quot (System/currentTimeMillis) 1000)
        body (assoc body :pubkey (util/bytes->hex-string pubkey)
                         :created_at now)
        id (make-id body)
        aux-rand (util/num->bytes 32 (biginteger (System/currentTimeMillis)))
        signature (ecc/do-sign id private-key aux-rand)
        event (assoc body :id (util/bytes->hex-string id)
                          :sig (util/bytes->hex-string signature))
        ]
    ["EVENT" event])
  )

(defn compose-metadata-event []
  (let [keys (get-event-state :keys)
        content (to-json {:name (:name keys)
                          :about (:about keys)
                          :picture (:picture keys)})
        body {:kind 0
              :tags []
              :content content}]
    (body->event body))
  )

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
              :content "more-speech contact list"}
        ]
    (body->event body))
  )

(defn make-event-reference-tags
  ([reply-to root]
   (if (or (nil? root) (= root reply-to))
     (make-event-reference-tags reply-to)
     [[:e (hexify root) "" "root"] [:e (hexify reply-to) "" "reply"]]))

  ([reply-to]
   (if (nil? reply-to)
     []
     [[:e (hexify reply-to) "" "reply"]])))

(defn make-people-reference-tags [pubkey reply-to-or-nil]
  (if (nil? reply-to-or-nil)
    []
    (let [event-map (get-event-state :text-event-map)
          parent-event-id reply-to-or-nil
          parent-event (get event-map parent-event-id)
          parent-tags (:tags parent-event)
          people-ids (map second (filter #(= :p (first %)) parent-tags))
          parent-author (:pubkey parent-event)
          people-ids (conj people-ids (hexify parent-author))
          people-ids (remove #(= (hexify pubkey) %) people-ids)]
      (map #(vector :p %) people-ids))))

(defn make-subject-tag [subject]
  (if (empty? (.trim subject))
    []
    [[:subject subject]]))


(defn get-reply-root [reply-to-or-nil]
  (if (nil? reply-to-or-nil)
    nil
    (let [reply-id reply-to-or-nil
          event-map (get-event-state :text-event-map)
          replied-to-event (get event-map reply-id)
          [root _mentions _referent] (get-references replied-to-event)]
      root)))

(defn find-user-id [user-name]
  (let [pet-pubkey (contact-list/get-pubkey-from-petname user-name)]
    (if (some? pet-pubkey)
      pet-pubkey
      (let [profiles (get-event-state :profiles)]
        (loop [pairs (vec profiles)]
          (if (empty? pairs)
            nil
            (let [pair (first pairs)]
              (if (= user-name (:name (second pair)))
                (first pair)
                (recur (rest pairs))))))))))

(defn abbreviate-pubkey [pubkey-string]
  (let [pubkey (util/hex-string->num pubkey-string)
        abbreviated-pubkey (str (subs pubkey-string 0 11) "-")
        event-context (:event-context @ui-context)]
    (swap! event-context assoc-in [:profiles pubkey] {:name abbreviated-pubkey})
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
        [emplacements tags] (make-emplacements references tags)]
    [(string/trim (apply str (interleave segments (conj emplacements ""))))
     tags]
    ))

(defn encrypt-if-direct-message [content tags]
  (if (re-find #"^D \#\[\d+\]" content)
    (let [reference-digits (re-find #"\d+" content)
          reference-index (Integer/parseInt reference-digits)
          p-tag (get tags reference-index)]
      (if (nil? p-tag)
        [content 1]
        (let [recipient-key (hex-string->num (second p-tag))
              sender-key (hex-string->num (get-in (get-event-state) [:keys :private-key]))
              shared-secret (SECP256K1/calculateKeyAgreement sender-key recipient-key)
              encrypted-content (SECP256K1/encrypt shared-secret content)]
          [encrypted-content 4])))
    [content 1])
  )

(defn compose-text-event
  ([subject text]
   (compose-text-event subject text nil))

  ([subject text reply-to-or-nil]
   (let [pubkey (get-event-state :pubkey)
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
  (let [send-chan (get-event-state :send-chan)]
    (async/>!! send-chan [:event event])))

(defn compose-and-send-text-event [source-event-or-nil subject message]
  (let [reply-to-or-nil (:id source-event-or-nil)
        event (compose-text-event subject message reply-to-or-nil)]
    (send-event event)))

(defn compose-and-send-metadata-event []
  (send-event (compose-metadata-event)))

(defn compose-and-send-contact-list [contact-list]
  (send-event (compose-contact-list contact-list)))



