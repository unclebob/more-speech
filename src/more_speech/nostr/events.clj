(ns more-speech.nostr.events
  (:require [clojure.spec.alpha :as s]
            [clojure.data.json :as json]
            [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.nostr.util :refer :all]
            [more-speech.nostr.elliptic-signature :as ecc]
            [more-speech.nostr.util :as util]
            [more-speech.nostr.relays :as relays]
            [more-speech.nostr.contact-list :as contact-list]
            [more-speech.config :as config]
            [clojure.stacktrace :as st]
            )
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

(def event-agent (agent nil))

(defn to-json [o]
  (json/write-str o :escape-slash false :escape-unicode false))

(defn make-id
  "returns byte array of id given the clojure form of the body"
  [{:keys [pubkey created_at kind tags content]}]
  (let [id-event (to-json [0 pubkey created_at kind tags content])
        id (util/sha-256 (.getBytes id-event StandardCharsets/UTF_8))]
    id)
  )

(defn- pow2 [n] (reduce * (repeat n 2N)))

(defn make-id-with-pow
  "returns byte array and updated body of id given the clojure form of the body, and the
  POW constraint given in the number of preceding binary zeroes."
  [pow body]
  (let [limit (pow2 (- 256 pow))]
    (loop [nonce 0]
      (let [body (update-in body [:tags] concat [[:nonce (str nonce) (str pow)]])
            id (make-id body)
            id-num (bytes->num id)]
        (if (< id-num limit)
          [id body]
          (recur (inc nonce)))))))

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

(defn compute-id [event]
  (util/bytes->num
    (make-id
      {:pubkey (get event "pubkey")
       :created_at (get event "created_at")
       :kind (get event "kind")
       :tags (get event "tags")
       :content (get event "content")})))

(defn is-text-event? [event]
  (or (= (:kind event) 1)
      (= (:kind event) 4)))

(defn process-tag [tag]
  (when (and (seq tag) (seq (first tag)))
    (let [tag-type (first tag)
          tag-args (rest tag)
          tag-type (.replace tag-type \: \-)]
      (concat [(keyword tag-type)] tag-args))))

(defn process-tags [tags]
  (remove nil? (map process-tag tags)))

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

(def event-counter (atom {:total 0}))

(defn count-event [envelope url]
  (let [source (second envelope)
        key (str url "|" source)]
    (swap! event-counter update :total inc)
    (swap! event-counter update key #(inc (if (nil? %) 0 %)))
    (when (zero? (mod (:total @event-counter) 1000))
      (clojure.pprint/pprint @event-counter))))

(defn record-and-display-event [_agent envelope url]
  (count-event envelope url)
  (try
    (let [[_name _subscription-id inner-event :as _decoded-msg] envelope
          event (translate-event inner-event)
          id (:id event)
          computed-id (compute-id inner-event)
          ui-handler (get-event-state :event-handler)
          dup? (contains? (get-event-state :text-event-map) id)]
      (if (= id computed-id)
        (let [event (decrypt-dm-event event)]
          (when (not (:private event))
            (swap! (:event-context @ui-context) process-event event url)
            (when (and (not dup?)
                       (is-text-event? event))
              (handle-text-event ui-handler event))))
        (prn 'id-mismatch url 'computed-id (util/num32->hex-string computed-id) envelope)))
    (catch Exception e
      (do (prn `record-and-display-event url (.getMessage e))
          (prn "--on event: " envelope)
          (st/print-stack-trace e)))))

;--------called externally by article-tree

(defn get-root-of-thread [id]
  (let [messages (get-event-state :text-event-map)
        event (get messages id)
        [root _ _] (get-references event)]
    (if (some? root) root id)))

(defn select-event [event-state tab-index id]
  (swap! ui-context assoc :selected-tab tab-index)
  (if-not (:backing-up event-state)
    (-> event-state
        (update :read-event-ids conj id)
        (update :event-history conj [tab-index id])
        (assoc :selected-event id :back-count 0))
    (-> event-state (assoc :selected-event id :backing-up false))))

;-------- For Tests.

(defn chronological-event-comparator [[i1 t1] [i2 t2]]
  (if (= i1 i2)
    0
    (compare t2 t1)))

(defn make-chronological-text-events []
  (sorted-set-by chronological-event-comparator))