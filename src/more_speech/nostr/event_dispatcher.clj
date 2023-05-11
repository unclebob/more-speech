(ns more-speech.nostr.event-dispatcher
  (:require [clojure.data.json :as json]
            [more-speech.config :as config :refer [get-db]]
            [more-speech.db.gateway :as gateway]
            [more-speech.logger.default :refer [log-pr]]
            [more-speech.mem :refer :all]
            [more-speech.nostr.contact-list :as contact-list]
            [more-speech.nostr.elliptic-signature :as ecc]
            [more-speech.nostr.events :as events]
            [more-speech.nostr.relays :as relays]
            [more-speech.nostr.util :refer :all :as util]
            [more-speech.nostr.zaps :as zaps])
  (:import (ecdhJava SECP256K1)))

(defprotocol event-handler
  (handle-text-event [handler event])
  (immediate-add-text-event [handler event]))

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
          name (get profile "name")
          about (get profile "about" "")
          picture (get profile "picture" "")
          lud16 (get profile "lud16")
          nip05 (get profile "nip05")
          lud06 (get profile "lud06")
          website (get profile "website")
          banner (get profile "banner")
          display-name (get profile "display_name")
          possible-alias (if (empty? display-name)
                           ""
                           (str "-" display-name))
          name (if (empty? name) possible-alias name)
          name (add-suffix-for-duplicate pubkey (fix-name name))
          profile-doc {:name name
                       :about about
                       :picture picture
                       :banner banner
                       :display-name display-name
                       :website website
                       :lud06 lud06
                       :lud16 lud16
                       :nip05 nip05
                       :created-at created-at}
          old-profile (gateway/get-profile db pubkey)
          old-date (get old-profile :created-at)]
      (when (or (nil? old-date)
                 (> created-at old-date))
        (gateway/add-profile db pubkey profile-doc)))
    (catch Exception e
      (log-pr 1 'json-exception-process-name-event-ignored (.getMessage e))
      (log-pr 1 event))))

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
        valid? (if (contains? config/kinds-not-to-validate kind)
                 true
                 (ecc/do-verify (util/num->bytes 32 id)
                                (util/num->bytes 32 pubkey)
                                (util/num->bytes 64 sig)))]
    (if (not valid?)
      (log-pr 1 'signature-verification-failed url event)
      (condp = kind
        0 (process-name-event db event)
        1 (process-text-event db event url)
        2 (process-server-recommendation event)
        3 (contact-list/process-contact-list db event)
        4 (process-text-event db event url)
        7 (process-reaction db event)
        9735 (zaps/process-zap-receipt event)
        nil))))

(defn decrypt-his-dm [event]
  (let [p-tags (filter #(= :p (first %)) (:tags event))
        my-tag (filter #(= (unhexify (second %))
                           (get-mem :pubkey))
                       p-tags)]
    (if (empty? my-tag)
      (assoc event :private true)                           ;I'm not the recipient.
      (let [his-key (biginteger (:pubkey event))
            priv-key (biginteger (unhexify (get-mem [:keys :private-key])))
            shared-secret (SECP256K1/calculateKeyAgreement priv-key his-key)
            decrypted-content (SECP256K1/decrypt shared-secret (:content event))
            event (assoc event :content decrypted-content)]
        event))))

(defn decrypt-my-dm [event]
  (let [p-tags (filter #(= :p (first %)) (:tags event))
        p-tag (first p-tags)
        his-key (biginteger (unhexify (second p-tag)))
        priv-key (biginteger (unhexify (get-mem [:keys :private-key])))
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

(defn count-event [url]
  (let [relay (re-find config/relay-pattern url)]
    (update-mem :websocket-backlog dec)
    (update-mem [:event-counter :total] inc)
    (update-mem [:event-counter relay] #(inc (if (nil? %) 0 %)))))

(defn handle-notification [envelope url]
  (set-mem [:relay-notice url] (with-out-str (clojure.pprint/pprint envelope)))
  (log-pr 2 'NOTICE url envelope))

(defn inc-if-nil [n]
  (if (nil? n) 1 (inc n)))

(defn add-relay-to-processed-event-ids [relays url]
  (if (some? relays)
    (conj relays url)
    #{url}))

(defn handle-duplicate-event [event id relays-already-sent-this-id url]
  (do
    (update-mem [:event-counter :dups] inc-if-nil)
    (when (is-text-event? event)
      (when-not (contains? relays-already-sent-this-id url)
        (update-mem [:processed-event-ids id] add-relay-to-processed-event-ids url)
        (gateway/add-relays-to-event (get-db) id [url])))))

(defn validate-and-process-event [url envelope]
  (let [[_name _subscription-id inner-event :as _decoded-msg] envelope
        event (translate-event inner-event)
        id (:id event)
        relays-already-sent-this-id (get-mem [:processed-event-ids id])]
    (update-mem [:event-counter :kinds (:kind event)] inc-if-nil)
    (if (some? relays-already-sent-this-id)
      (handle-duplicate-event event id relays-already-sent-this-id url)
      (let [computed-id (compute-id inner-event)
            ui-handler (get-mem :event-handler)]
        (update-mem :processed-event-ids assoc id #{url})
        (if (= id computed-id)
          (let [event (decrypt-dm-event event)]
            (when (not (:private event))
              (process-event event url)
              (when (is-text-event? event)
                (handle-text-event ui-handler event))))
          (log-pr 2 'id-mismatch url 'computed-id (util/num32->hex-string computed-id) envelope))))))

(defn try-validate-and-process-event [url envelope]
  (try
    (validate-and-process-event url envelope)
    (catch Exception e
      (do (log-pr 1 'handle-event url (.getMessage e))
          (log-pr 1 "--on event: " envelope)))))

(defn handle-event [_agent envelope url]
  (count-event url)
  (if (not= "EVENT" (first envelope))
    (handle-notification envelope url)
    (try-validate-and-process-event url envelope)))
