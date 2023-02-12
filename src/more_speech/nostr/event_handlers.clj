(ns more-speech.nostr.event-handlers
  (:require [clojure.data.json :as json]
            [more-speech.db.gateway :as gateway]
            [more-speech.nostr.events :as events]
            [more-speech.mem :refer :all]
            [more-speech.nostr.util :refer :all]
            [more-speech.nostr.elliptic-signature :as ecc]
            [more-speech.nostr.util :refer [unhexify]]
            [more-speech.nostr.relays :as relays]
            [more-speech.nostr.contact-list :as contact-list]
            [more-speech.config :as config :refer [get-db]]
            [clojure.stacktrace :as st]
            [more-speech.nostr.util :as util])
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

(defn add-suffix-for-duplicate
  ([pubkey name]
   (add-suffix-for-duplicate pubkey name 1))

  ([pubkey name max]
  (let [id-of-name (gateway/get-id-from-username (get-db) name)]
    (if (or (nil? id-of-name) (= id-of-name pubkey))
      name
      (recur pubkey (str name (inc (rand-int max))) (* 10 max))))))

(defn process-name-event [db {:keys [_id pubkey created-at _kind _tags content _sig] :as event}]
  (try
    (let [profile (json/read-str content)
          name (get profile "name" "tilt")
          about (get profile "about" "")
          picture (get profile "picture" "")
          name (add-suffix-for-duplicate pubkey (fix-name name))
          profile {:name name
                   :about about
                   :picture picture
                   :created-at created-at}]
      (gateway/add-profile db pubkey profile))
    (catch Exception e
      (prn 'json-exception-process-name-event-ignored (.getMessage e))
      (prn event))))

(defn add-cross-reference [db event]
  (let [[_ _ referent] (events/get-references event)]
    (when (and (some? referent)
               (gateway/event-exists? db referent))
      (gateway/add-reference-to-event db referent (:id event)))))

(defn add-event [db event urls]
  (let [id (:id event)
        time (:created-at event)]
    (when-not (gateway/event-exists? db id)
      (gateway/add-event db event)
      (add-cross-reference db event)
      (set-mem :days-changed (conj (get-mem :days-changed) (quot time 86400))))
    (gateway/add-relays-to-event db id urls)))

(defn process-text-event [db event url]
  (add-event db event [url])
  (relays/add-recommended-relays-in-tags event))

(defn get-last-x-tag [x tags]
  (let [x-tags (filter #(= x (first %)) tags)]
    (when (zero? (count x-tags))
      (throw (Exception. (str "no " x "tags"))))
    (last x-tags)))

(defn get-last-p-tag [tags]
  (get-last-x-tag :p tags))

(defn get-last-e-tag [tags]
  (get-last-x-tag :e tags))

(defn process-reaction [db event]
  (try
    (let [{:keys [content tags]} event
          last-e-tag (get-last-e-tag tags)
          target-id (unhexify (second last-e-tag))
          reactor (:pubkey event)]
      (when (gateway/event-exists? db target-id)
        (gateway/add-reaction db target-id reactor content)))
    (catch Exception _e))
  )

(defn process-server-recommendation [event]
  (relays/add-relay (:content event)))

(defn process-event [event url]
  (let [db (get-db)
        {:keys [id pubkey _created-at kind _tags _content sig]} event
        valid? (ecc/do-verify (util/num->bytes 32 id)
                              (util/num->bytes 32 pubkey)
                              (util/num->bytes 64 sig))]
    (if (not valid?)
      (prn 'signature-verification-failed url event)
      (condp = kind
        0 (process-name-event db event)
        1 (process-text-event db event url)
        2 (process-server-recommendation event)
        3 (contact-list/process-contact-list db event)
        4 (process-text-event db event url)
        7 (process-reaction db event)
        nil))))

(defn decrypt-his-dm [event]
  (let [p-tags (filter #(= :p (first %)) (:tags event))
        my-tag (filter #(= (unhexify (second %))
                           (get-mem :pubkey))
                       p-tags)]
    (if (empty? my-tag)
      (assoc event :private true)                           ;I'm not the recipient.
      (let [his-key (:pubkey event)
            priv-key (unhexify (get-mem [:keys :private-key]))
            shared-secret (SECP256K1/calculateKeyAgreement priv-key his-key)
            decrypted-content (SECP256K1/decrypt shared-secret (:content event))
            event (assoc event :content decrypted-content)]
        event))))

(defn decrypt-my-dm [event]
  (let [p-tags (filter #(= :p (first %)) (:tags event))
        p-tag (first p-tags)
        his-key (unhexify (second p-tag))
        priv-key (unhexify (get-mem [:keys :private-key]))
        shared-secret (SECP256K1/calculateKeyAgreement priv-key his-key)
        decrypted-content (SECP256K1/decrypt shared-secret (:content event))
        event (assoc event :content decrypted-content)]
    event))

(defn decrypt-dm-event [event]
  (if (= 4 (:kind event))
    (let [author-key (:pubkey event)
          event (assoc event :dm true)]
      (if (= author-key (get-mem :pubkey))
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
          tag-type (if (clojure.string/blank? tag-type) "blank" tag-type)
          tag-args (rest tag)
          tag-type (.replace tag-type \: \-)]
      (concat [(keyword tag-type)] tag-args))))

(defn process-tags [tags]
  (remove nil? (map process-tag tags)))

(defn translate-event [event]
  (let [id (unhexify (get event "id"))
        pubkey (unhexify (get event "pubkey"))
        sig (unhexify (get event "sig"))]
    {:id id
     :pubkey pubkey
     :created-at (get event "created_at")
     :kind (get event "kind")
     :content (get event "content")
     :sig sig
     :tags (process-tags (get event "tags"))}))

(def event-counter (atom {:total 0}))

(defn count-event [envelope url]
  (let [type (first envelope)
        source (second envelope)
        key (str url "|" source)]
    (when (= type "EVENT")
      (swap! event-counter update :total inc)
      (swap! event-counter update key #(inc (if (nil? %) 0 %)))
      (when (zero? (mod (:total @event-counter) 1000))
        (prn 'websocket-backlog @config/websocket-backlog)
        (clojure.pprint/pprint @event-counter)))))

(defn handle-notification [envelope url]
  (prn 'NOTICE url envelope))

(defn handle-event [_agent envelope url]
  (swap! config/websocket-backlog dec)
  (if (and (not= "OK" (first envelope))
           (not (.startsWith (second envelope) "more-speech")))
    (prn 'strange-message-source url envelope)
    (count-event envelope url))
  (if (not= "EVENT" (first envelope))
    (handle-notification envelope url)
    (try
      (let [[_name _subscription-id inner-event :as _decoded-msg] envelope
            event (translate-event inner-event)
            id (:id event)
            computed-id (compute-id inner-event)
            ui-handler (get-mem :event-handler)
            dup? (some? (gateway/get-event (get-db) id))
            ]
        (if (= id computed-id)
          (let [event (decrypt-dm-event event)]
            (when (not (:private event))
              (process-event event url)
              (when (and (not dup?)
                         (is-text-event? event))
                (handle-text-event ui-handler event))))
          (prn 'id-mismatch url 'computed-id (util/num32->hex-string computed-id) envelope)))
      (catch Exception e
        (do (prn `handle-event url (.getMessage e))
            (prn "--on event: " envelope)
            (st/print-stack-trace e))))))
