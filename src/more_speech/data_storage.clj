(ns more-speech.data-storage
  (:require [more-speech.config :as config]
            [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.nostr.events :as events]
            [more-speech.nostr.event-handlers :as handlers]
            [more-speech.nostr.relays :as relays]
            [more-speech.ui.swing.tabs :as tabs]
            [more-speech.user-configuration :as user-configuration]
            [clojure.string :as string]
            [more-speech.nostr.util :as util]
            [more-speech.ui.formatter-util :as fu])
  (:import (java.util Date TimeZone Locale)
           (java.text SimpleDateFormat)))


(defn write-configuration []

  (spit @config/profiles-filename
        (with-out-str
          (clojure.pprint/pprint (get-event-state :profiles))))
  (spit @config/read-event-ids-filename
        (with-out-str
          (clojure.pprint/pprint (get-event-state :read-event-ids))))
  (spit @config/relays-filename
        (with-out-str
          (clojure.pprint/pprint (relays/relays-for-writing))))
  (spit @config/tabs-list-filename
        (with-out-str
          (clojure.pprint/pprint (get-event-state :tabs-list))))
  (spit @config/user-configuration-filename
        (with-out-str
          (clojure.pprint/pprint (get-event-state :user-configuration))))
  (spit @config/contact-lists-filename
        (with-out-str
          (clojure.pprint/pprint (get-event-state :contact-lists))))
  )

(defn write-messages []
  (let [event-context (:event-context @ui-context)]
    (spit @config/messages-filename
          (with-out-str
            (clojure.pprint/pprint (:text-event-map @event-context))))))

(defn load-configuration []
  (let [keys (read-string (slurp @config/keys-filename))
        pubkey (util/hex-string->num (:public-key keys))
        read-event-ids (read-string (slurp @config/read-event-ids-filename))
        profiles (read-string (slurp @config/profiles-filename))
        tabs-list (tabs/ensure-tab-list-has-all
                    (read-string (slurp @config/tabs-list-filename)))
        user-configuration (user-configuration/validate
                             (read-string (slurp @config/user-configuration-filename)))
        contact-lists (read-string (slurp @config/contact-lists-filename))

        event-context (events/make-event-context {:keys keys
                                                  :pubkey pubkey
                                                  :profiles profiles
                                                  :read-event-ids read-event-ids
                                                  :tabs-list tabs-list
                                                  :user-configuration user-configuration
                                                  :contact-lists contact-lists
                                                  })]
    (swap! ui-context assoc :event-context event-context)
    (relays/load-relays-from-file @config/relays-filename)))

(defn load-events [old-events handler]
  (doseq [event old-events]
    (let [urls (:relays event)]
      (handlers/add-event (get-db) event urls)
      (handlers/handle-text-event handler event))))

(defn read-old-events [handler]
  (let [old-events (vals (read-string (slurp @config/messages-filename)))
        creation-times (map :created-at old-events)]
    (load-events old-events handler)
    (if (empty? creation-times)
      (-> (System/currentTimeMillis) (quot 1000) (- 86400))
      (apply max creation-times))))

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
   (let [event-context (:event-context @ui-context)
         message-map (:text-event-map @event-context)
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
  (let [days-changed (get-event-state :days-changed)
        earliest-loaded-time (get-event-state :earliest-loaded-time)
        first-day-loaded (quot earliest-loaded-time 86400)
        days-to-write (set (filter #(>= % first-day-loaded) days-changed))
        daily-partitions (partition-messages-by-day (get-event-state :text-event-map))
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

(defn read-in-last-n-days [n handler]
  (prn 'read-in-last-n-days 'starting)
  (let [message-directory (clojure.java.io/file @config/messages-directory)
        files (.listFiles message-directory)
        file-names (for [file files] (.getName file))
        file-names (filter is-message-file? file-names)
        file-names (take-last n (sort file-names))
        first-file-name (first file-names)
        last-file-name (last file-names)
        _ (prn 'read-in-last-n-days 'last-file-name last-file-name)
        first-time (time-from-file-name first-file-name)
        last-time (if (nil? last-file-name)
                    nil
                    (get-last-event-time last-file-name))
        _ (prn 'read-in-last-n-days 'last-time (fu/format-time last-time))
        now (quot (System/currentTimeMillis) 1000)
        last-time (if (nil? last-time) (- now 86400) last-time)
        first-time (if (nil? first-time) now first-time)]
    (future
      (doseq [file-name (reverse file-names)]
        (prn 'reading file-name)
        (let [old-events (read-string (slurp (str @config/messages-directory "/" file-name)))]
          (try
            (load-events old-events handler)
            (catch Exception e
              (prn 'EXCEPTION 'read-in-last-n-days 'reading file-name 'failed)
              (prn e)))))
      (swap! (:event-context @ui-context)
             assoc :days-changed #{(quot last-time 86400)}
             :earliest-loaded-time first-time))
    last-time))

