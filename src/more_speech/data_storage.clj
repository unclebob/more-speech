(ns more-speech.data-storage
  (:require [clojure.string :as string]
            [more-speech.bech32 :as bech32]
            [more-speech.config :as config :refer [get-db]]
            [more-speech.db.gateway :as gateway]
            [more-speech.db.in-memory :as in-memory]
            [more-speech.db.xtdb :as xtdb]
            [more-speech.logger.default :refer [log-pr]]
            [more-speech.mem :refer :all]
            [more-speech.nostr.event-dispatcher :as handlers]
            [more-speech.nostr.relays :as relays]
            [more-speech.nostr.util :as util]
            [more-speech.ui.formatter-util :as fu]
            [more-speech.ui.swing.tabs :as tabs]
            [more-speech.user-configuration :as user-configuration]
            [more-speech.util.files :refer :all]
            [xtdb.api :as xt])
  (:import (java.text SimpleDateFormat)
           (java.util Date Locale TimeZone)))

(defn write-relays []
  (log-pr 2 'writing-relays)
  (let [relay-output (with-out-str
                       (clojure.pprint/pprint (relays/relays-for-writing)))]
    (if (not (config/is-test-run?))
      (spit @config/relays-filename relay-output)
      (log-pr 2 'relays-not-written relay-output))))

(defn write-keys [keys]
  (let [private-key (:private-key keys)
        wallet-connect (:wallet-connect keys)
        password (:password keys)
        encoded-password (if (empty? password)
                           password
                           (bech32/encode-str "pw" password))
        private-key (if (empty? password)
                      private-key
                      (->> private-key
                           (util/xor-string password)
                           (bech32/encode-str "encoded")))
        wallet-connect (if (some? wallet-connect)
                         (if (empty? password)
                           wallet-connect
                           (->> wallet-connect
                                (util/xor-string password)
                                (bech32/encode-str "encoded")))
                         nil)
        keys (assoc keys :private-key private-key
                         :password encoded-password
                         :wallet-connect wallet-connect)
        keys-string (with-out-str (clojure.pprint/pprint keys))]
    (if (config/is-test-run?)
      (log-pr 2 'keys-not-written
              (if (empty? password)
                (dissoc keys :private-key :wallet-connect)
                keys))
      (spit @config/keys-filename keys-string))))

(defn write-tabs []
  (log-pr 2 'writing-tabs)
  (let [tabs-output (with-out-str (clojure.pprint/pprint (get-mem :tabs-list)))]
    (if-not (config/is-test-run?)
      (spit @config/tabs-list-filename tabs-output)
      (log-pr 2 'tabs-not-written tabs-output))))

(defn write-user-configuration []
  (log-pr 2 'writing-user-configuration)
  (let [user-config-output (with-out-str
                (clojure.pprint/pprint (user-configuration/get-config)))]
    (if-not (config/is-test-run?)
      (spit @config/user-configuration-filename user-config-output)
      (log-pr 2 'user-config-not-written user-config-output))))

(defn write-configuration []
  (write-relays)
  (write-tabs)
  (write-user-configuration)
  (log-pr 2 'configuration-written)
  )

(defn read-profiles []
  (if (and (= :in-memory @config/db-type)
           (file-exists? @config/profiles-filename))
    (read-string (slurp @config/profiles-filename))
    {}))

(defn read-contact-lists []
  (if (and (= :in-memory @config/db-type)
           (file-exists? @config/contact-lists-filename))
    (read-string (slurp @config/contact-lists-filename))
    {}))

(defn read-keys []
  (let [keys (read-string (slurp @config/keys-filename))
        pubkey (util/hex-string->num (:public-key keys))
        pw (:password keys)
        pw (if (empty? pw) nil
                           (bech32/address->str pw))
        private-key (:private-key keys)
        private-key (if (some? pw)
                      (util/xor-string pw (bech32/address->str private-key))
                      private-key)
        wallet-connect (:wallet-connect keys)
        wallet-connect (if (some? wallet-connect)
                         (if (some? pw)
                           (util/xor-string pw (bech32/address->str wallet-connect))
                           wallet-connect)
                         nil)]
    (set-mem :keys (assoc keys :private-key private-key
                               :password pw
                               :wallet-connect wallet-connect))
    (set-mem :pubkey pubkey)
    )
  )

(defn load-configuration []
  (let [tabs-list (tabs/ensure-tab-list-has-all
                    (read-string (slurp @config/tabs-list-filename)))
        user-configuration (user-configuration/validate
                             (read-string (slurp @config/user-configuration-filename)))
        profiles (read-profiles)
        contact-lists (read-contact-lists)]
    (read-keys)
    (when (= :in-memory @config/db-type)
      (swap! in-memory/db assoc :contact-lists contact-lists)
      (swap! in-memory/db assoc :profiles profiles))
    (set-mem :tabs-list tabs-list)
    (set-mem :user-configuration user-configuration)
    (set-mem :event-history [])
    (set-mem :back-count 0)
    (set-mem :processed-event-ids {})
    (set-mem :websocket-backlog 0)
    (set-mem :event-counter {:total 0})
    (set-mem :incoming-events 0)

    (if (config/is-test-run?)
      (reset! relays config/test-relays)
      (relays/load-relays-from-file @config/relays-filename))))

(defn load-events [old-events handler]
  (loop [events old-events
         event-count 0]
    (if (empty? events)
      (log-pr 2 'done-loading-events)
      (let [event (first events)]
        (when (zero? (rem event-count 100))
          (log-pr 2 event-count 'events-loaded (fu/format-time (:created-at event)) 'backlog (get-mem :websocket-backlog))
          (Thread/sleep 5000))
        (try
          (handlers/handle-text-event handler event)
          (if (> (get-mem :websocket-backlog) 10)
            (Thread/sleep 100)
            (Thread/sleep 50))                              ;take a breath
          (catch Exception e
            (log-pr 1 'EXCEPTION 'load-events e)))
        (recur (rest events) (inc event-count))))))

(defn partition-messages-by-day [message-map]
  (let [messages (sort-by :created-at (vals message-map))
        messages (partition-by #(quot (:created-at %) 86400) messages)
        messages (map #(vector (quot (:created-at (first %)) 86400) %) messages)]
    messages))

(defn file-name-from-day [day]
  (let [time (* day 86400000)
        date (Date. (long time))
        date-format (SimpleDateFormat. "ddMMMyy" Locale/US)]
    (.setTimeZone date-format (TimeZone/getTimeZone "UTC"))
    (str day "-" (.format date-format date))))

(defn write-messages-by-day
  ([]
   (let [message-map (get @in-memory/db :text-event-map)
         daily-partitions (partition-messages-by-day message-map)]
     (write-messages-by-day daily-partitions)))

  ([daily-partitions]
   (let []
     (doseq [day-partition daily-partitions]
       (let [file-name (file-name-from-day (first day-partition))]
         (log-pr 2 'writing file-name)
         (spit (str @config/messages-directory "/" file-name)
               (with-out-str
                 (clojure.pprint/pprint
                   (second day-partition)))))))))

(defn write-changed-days []
  (log-pr 2 'writing-events-for-changed-days)
  (let [days-changed (get-mem :days-changed)
        earliest-loaded-time (get-mem :earliest-loaded-time)
        _ (log-pr 2 'earliest-loaded-time earliest-loaded-time)
        first-day-loaded (quot earliest-loaded-time 86400)
        days-to-write (set (filter #(>= % first-day-loaded) days-changed))
        daily-partitions (partition-messages-by-day (get @in-memory/db :text-event-map))
        changed-partitions (filter #(contains? days-to-write (first %)) daily-partitions)]
    (write-messages-by-day changed-partitions)))

(defn time-from-file-name [file-name]
  (if (nil? file-name)
    nil
    (try
      (let [parts (string/split file-name #"\-")]
        (* 86400 (Integer/parseInt (first parts))))
      (catch Exception _e
        nil)))
  )

(defn get-last-event-time [file-name]
  (let [events (read-string (slurp (str @config/messages-directory "/" file-name)))
        last-event (last events)
        last-time (:created-at last-event)]
    last-time))

(defn is-message-file? [file-name]
  (re-matches #"\d+\-\d+\w+\d+" file-name))

(defn get-read-events []
  (let [db (get-db)
        now (quot (System/currentTimeMillis) 1000)
        start-time (int (- now (* config/days-to-read-messages-that-have-been-read 86400)))
        event-ids (gateway/get-ids-of-read-events-since db start-time)]
    (log-pr 2 'reading (count event-ids) 'read-messages)
    event-ids))

(defn load-event-history [handler]
  (log-pr 2 'load-event-history 'starting)
  (let [db (get-db)
        read-event-ids (get-read-events)
        now (quot (System/currentTimeMillis) 1000)
        start-time (int (- now (* config/days-to-read 86400)))
        all-event-ids (gateway/get-event-ids-since db start-time)
        event-ids (concat read-event-ids all-event-ids)
        events (map #(gateway/get-event db %) event-ids)
        times (map :created-at events)
        last-time (if (empty? times)
                    (- now 86400)
                    (apply max times))
        _ (log-pr 2 'load-event-history 'last-time (fu/format-time last-time))]
    (future (load-events events handler))
    (log-pr 2 'reading-files-complete)
    last-time))

(defn get-events-since [db since]
  (with-open
    [stream (xt/open-q (xt/db db)
                       '{:find [(pull e [*])]
                         :timeout 86400000
                         :in [when]
                         :where [[e :xt/id id]
                                 [(get id :type) type]
                                 [(= type :event)]
                                 [e :created-at t]
                                 [(> t when)]]}
                       since)]
    (loop [events-in (iterator-seq stream)
           events-out []
           n 1]
      (if (empty? events-in)
        events-out
        (let [event (first events-in)]
          (when (zero? (mod n 10000))
            (log-pr 1 'ingested n 'events))
          (recur (rest events-in) (conj events-out (first event)) (inc n)))))))

(defn get-profiles-since [db since]
  (with-open
    [stream (xt/open-q (xt/db db)
                       '{:find [(pull e [*])]
                         :timeout 86400000
                         :in [when]
                         :where [[e :xt/id id]
                                 [(get id :type) type]
                                 [(= type :profile)]
                                 [e :created-at t]
                                 [(> t when)]]}
                       since)]
    (loop [profiles-in (iterator-seq stream)
           profiles-out []
           n 1]
      (if (empty? profiles-in)
        profiles-out
        (let [profile (first profiles-in)]
          (when (zero? (mod n 1000))
            (log-pr 1 'ingested n 'profiles))
          (recur (rest profiles-in) (conj profiles-out (first profile)) (inc n)))))))

(defn get-contacts-since [db since]
  (with-open
    [stream (xt/open-q (xt/db db)
                       '{:find [(pull e [*])]
                         :timeout 86400000
                         :in [when]
                         :where [[e :xt/id id]
                                 [(get id :type) type]
                                 [(= type :contacts)]
                                 [e :created-at t]
                                 [(> t when)]
                                 ]}
                       since)]
    (loop [contacts-in (iterator-seq stream)
           contacts-out []
           n 1]
      (if (empty? contacts-in)
        contacts-out
        (let [contact-list (first contacts-in)]
          (when (zero? (mod n 5000))
            (log-pr 1 'ingested n 'contact-lists))
          (recur (rest contacts-in) (conj contacts-out (first contact-list)) (inc n)))))))

(defn put-events [db events]
  (log-pr 1 'putting (count events) 'events)
  (xt/submit-tx db (map #(vector ::xt/put %) events)))

(defn put-profiles [db profiles]
  (log-pr 1 'putting (count profiles) 'profiles)
  (xt/submit-tx db (map #(vector ::xt/put %) profiles)))

(defn fix-contact-list [contact-list]
  (if (contains? contact-list :created-at)
    contact-list
    (assoc contact-list :created-at (util/get-now))))

(defn put-contacts [db contacts]
  (log-pr 1 'putting (count contacts) 'contacts)
  (xt/submit-tx db (map #(vector ::xt/put (fix-contact-list %)) contacts)))

(defn compress []
  (log-pr 1 'Compressing)
  (let [now (util/get-now)
        event-since (- now (* 14 86400))
        profiles-since (- now (* 90 86400))
        contacts-since (- now (* 90 86400))
        prod-db (xtdb/start-xtdb! config/prod-db)
        temp-db (xtdb/start-xtdb! config/temp-db)]
    (put-events temp-db (get-events-since prod-db event-since))
    (put-profiles temp-db (get-profiles-since prod-db profiles-since))
    (put-contacts temp-db (get-contacts-since prod-db contacts-since))
    (xt/sync temp-db)
    (log-pr 1 'renaming config/prod-db (str config/prod-db "-old"))
    (rename-file config/prod-db (str config/prod-db "-old"))
    (rename-file config/temp-db config/prod-db)))

