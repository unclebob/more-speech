(ns more-speech.data-storage
  (:require [more-speech.config :as config :refer [get-db]]
            [more-speech.mem :refer :all]
            [more-speech.nostr.event-handlers :as handlers]
            [more-speech.nostr.relays :as relays]
            [more-speech.ui.swing.tabs :as tabs]
            [more-speech.user-configuration :as user-configuration]
            [clojure.string :as string]
            [more-speech.nostr.util :as util]
            [more-speech.ui.formatter-util :as fu]
            [more-speech.db.in-memory :as in-memory]
            [more-speech.util.files :refer :all]
            [more-speech.db.gateway :as gateway])
  (:import (java.util Date TimeZone Locale)
           (java.text SimpleDateFormat)))


(defn write-configuration []
  (prn 'writing-relays)
  (spit @config/relays-filename
        (with-out-str
          (clojure.pprint/pprint (relays/relays-for-writing))))

  (prn 'writing-tabs)
  (spit @config/tabs-list-filename
        (with-out-str
          (clojure.pprint/pprint (get-mem :tabs-list))))

  (prn 'writing-user-configuration)
  (spit @config/user-configuration-filename
        (with-out-str
          (clojure.pprint/pprint (user-configuration/get-config))))
  (prn 'configuration-written)
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

(defn load-configuration []
  (let [keys (read-string (slurp @config/keys-filename))
        pubkey (util/hex-string->num (:public-key keys))
        tabs-list (tabs/ensure-tab-list-has-all
                    (read-string (slurp @config/tabs-list-filename)))
        user-configuration (user-configuration/validate
                             (read-string (slurp @config/user-configuration-filename)))
        profiles (read-profiles)
        contact-lists (read-contact-lists)
        ]
    (when (= :in-memory @config/db-type)
      (swap! in-memory/db assoc :contact-lists contact-lists)
      (swap! in-memory/db assoc :profiles profiles))
    (set-mem :keys keys)
    (set-mem :pubkey pubkey)
    (set-mem :tabs-list tabs-list)
    (set-mem :user-configuration user-configuration)
    (set-mem :event-history [])
    (set-mem :back-count 0)
    (if (config/is-test-run?)
      (reset! relays/relays config/test-relays)
      (relays/load-relays-from-file @config/relays-filename))))

(defn load-events [old-events handler]
  (loop [events old-events
         event-count 0]
    (if (empty? events)
      (prn 'done-loading-events)
      (let [event (first events)]
        (when (zero? (rem event-count 100))
          (prn event-count 'events-loaded (fu/format-time (:created-at event)) 'backlog @config/websocket-backlog)
          (Thread/sleep 5000))
        (try
          (handlers/handle-text-event handler event)
          (if (> @config/websocket-backlog 10)
            (Thread/sleep 100)
            (Thread/sleep 50))                              ;take a breath
          (catch Exception e
            (prn 'EXCEPTION 'load-events)
            (prn e)))
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
    (str day "-" (.format date-format date))
    ))

(defn write-messages-by-day
  ([]
   (let [message-map (get @in-memory/db :text-event-map)
         daily-partitions (partition-messages-by-day message-map)]
     (write-messages-by-day daily-partitions)))

  ([daily-partitions]
   (let []
     (doseq [day-partition daily-partitions]
       (let [file-name (file-name-from-day (first day-partition))]
         (prn 'writing file-name)
         (spit (str @config/messages-directory "/" file-name)
               (with-out-str
                 (clojure.pprint/pprint
                   (second day-partition)))))))))

(defn write-changed-days []
  (prn 'writing-events-for-changed-days)
  (let [days-changed (get-mem :days-changed)
        earliest-loaded-time (get-mem :earliest-loaded-time)
        _ (prn 'earliest-loaded-time earliest-loaded-time)
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
    (prn 'reading (count event-ids) 'read-messages)
    event-ids))

(defn load-event-history [handler]
  (prn 'load-event-history 'starting)
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
        _ (prn 'load-event-history 'last-time (fu/format-time last-time))]
    (future (load-events events handler))
    (prn 'reading-files-complete)
    last-time))

