(ns more-speech.nostr.event-handlers
  (:require [clojure.data.json :as json]
            [more-speech.nostr.events :as events]
            [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.nostr.util :refer :all]
            [more-speech.nostr.elliptic-signature :as ecc]
            [more-speech.nostr.util :as util]
            [more-speech.nostr.relays :as relays]
            [more-speech.nostr.contact-list :as contact-list]
            [more-speech.config :as config]
            [clojure.stacktrace :as st]
            )
  (:import (ecdhJava SECP256K1)))

(defprotocol event-handler
  (handle-text-event [handler event])
  (update-relay-panel [handler]))

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
  (let [[_ _ referent] (events/get-references event)]
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
    (events/make-id
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

(defn handle-event [_agent envelope url]
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
      (do (prn `handle-event url (.getMessage e))
          (prn "--on event: " envelope)
          (st/print-stack-trace e)))))