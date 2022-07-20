(ns more-speech.data-storage
  (:require [more-speech.config :as config]
            [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.nostr.events :as events]
            [more-speech.nostr.relays :as relays]
            [more-speech.ui.swing.tabs :as tabs]
            [clojure.string :as string])
  (:import (java.util Date TimeZone)
           (java.text SimpleDateFormat)))


(defn write-configuration []
  (let [event-context (:event-context @ui-context)]
    (spit @config/profiles-filename
          (with-out-str
            (clojure.pprint/pprint (:profiles @event-context))))
    (spit @config/read-event-ids-filename
          (with-out-str
            (clojure.pprint/pprint (:read-event-ids @event-context))))
    (spit @config/relays-filename
          (with-out-str
            (clojure.pprint/pprint (relays/relays-for-writing))))))

(defn write-messages []
  (let [event-context (:event-context @ui-context)]
    (spit @config/messages-filename
          (with-out-str
            (clojure.pprint/pprint (:text-event-map @event-context))))))

(defn load-configuration []
  (let [keys (read-string (slurp @config/keys-filename))
        read-event-ids (read-string (slurp @config/read-event-ids-filename))
        profiles (read-string (slurp @config/profiles-filename))
        tabs-list (tabs/ensure-tab-list-has-all
                    (read-string (slurp @config/tabs-list-filename)))
        event-context (events/make-event-context {:keys keys
                                                  :profiles profiles
                                                  :read-event-ids read-event-ids
                                                  :tabs-list tabs-list
                                                  })]
    (swap! ui-context assoc :event-context event-context)
    (relays/load-relays-from-file @config/relays-filename)))

(defn load-events [old-events event-context handler]
  (doseq [event old-events]
    (let [url (first (:relays event))]
      (swap! event-context events/add-event event url)
      (events/handle-text-event handler event))))

(defn read-old-events [event-context handler]
  (let [old-events (vals (read-string (slurp @config/messages-filename)))
        creation-times (map :created-at old-events)]
    (load-events old-events event-context handler)
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
        date-format (SimpleDateFormat. "ddMMMyy")]
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

(defn write-changed-days [event-context]
  (let [days-changed (:days-changed @event-context)
        first-day-loaded (quot (:earliest-loaded-time @event-context) 86400)
        days-to-write (set (filter #(>= % first-day-loaded) days-changed))
        daily-partitions (partition-messages-by-day (:text-event-map @event-context))
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

(defn is-message-file? [file-name]
  (re-matches #"\d+\-\d+\w+\d+" file-name))

(defn read-in-last-n-days [n event-context handler]
  (let [message-directory (clojure.java.io/file @config/messages-directory)
        files (.listFiles message-directory)
        file-names (for [file files] (.getName file))
        file-names (filter is-message-file? file-names)
        file-names (take-last n (sort file-names))
        first-file-name (first file-names)
        last-file-name (last file-names)
        first-time (time-from-file-name first-file-name)
        last-time (time-from-file-name last-file-name)
        now (/ (System/currentTimeMillis) 1000)
        last-time (if (nil? last-time) now last-time)
        first-time (if (nil? first-time) now first-time)]
    (doseq [file-name file-names]
      (prn 'reading file-name)
      (let [old-events (read-string (slurp (str @config/messages-directory "/" file-name)))]
        (load-events old-events event-context handler)))
    (swap! event-context assoc :days-changed #{} :earliest-loaded-time first-time)
    last-time))

