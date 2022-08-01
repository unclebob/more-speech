(ns more-speech.migrator
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [more-speech.config :refer [migration-filename]]
            [more-speech.config :as config]
            [more-speech.nostr.util :as util]
            [more-speech.nostr.elliptic-signature :as ecc]
            [more-speech.nostr.events :as events]
            [more-speech.data-storage :as data-storage]
            [more-speech.data-storage :as data-storage]
            [more-speech.user-configuration :as user-configuration])
  (:import (java.security SecureRandom)))

(defn file-exists? [fname]
  (.exists (io/file fname)))

(defn is-directory? [fname]
  (.isDirectory (io/file fname)))

(defn delete-file [fname]
  (when (file-exists? fname)
    (io/delete-file fname)))

(defn make-private-key []
  (let [gen (SecureRandom.)
        key-bytes (byte-array 32)
        _ (.nextBytes gen key-bytes)]
    key-bytes))

;---The Migrations

(defn initial-migration []
  (when-not (file-exists? @config/private-directory)
    (.mkdir (io/file @config/private-directory)))
  (when-not (file-exists? @config/keys-filename)
    (let [private-key (make-private-key)
          public-key (ecc/get-pub-key private-key)
          temp-user-name (str "more-speech-" (rand-int 100000))]
      (spit @config/keys-filename {:name temp-user-name
                                   :about ""
                                   :picture ""
                                   :public-key (util/bytes->hex-string public-key)
                                   :private-key (util/bytes->hex-string private-key)})
      (println "A private and public key have been generated for you.")
      (println "Your temporary user name is: " temp-user-name)
      (println "Please edit private/keys to change it."))
    )
  (when-not (file-exists? @config/nicknames-filename)
    (spit @config/nicknames-filename {16r2ef93f01cd2493e04235a6b87b10d3c4a74e2a7eb7c3caf168268f6af73314b5 "unclebobmartin"}))
  (when-not (file-exists? @config/relays-filename)
    (spit @config/relays-filename {"wss://nostr-pub.wellorder.net"
                                   {:read true :write true}
                                   }))
  (when-not (file-exists? @config/read-event-ids-filename)
    (spit @config/read-event-ids-filename #{}))
  (when-not (file-exists? @config/tabs-filename)
    (spit @config/tabs-filename {:follow
                                 {:selected [16r2ef93f01cd2493e04235a6b87b10d3c4a74e2a7eb7c3caf168268f6af73314b5]
                                  :blocked []
                                  }}))
  )

;--- Migration 2 ---

(defn migration-2-fix-names []
  (let [nicknames (read-string (slurp @config/nicknames-filename))
        fixed-nicknames (apply hash-map (flatten (map (fn [v] (vector (first v) (events/fix-name (second v)))) nicknames)))]
    (spit @config/nicknames-filename fixed-nicknames)
    ))

;--- Migration 3 ---

(defn migration-3-add-messages-directory []
  (when-not (file-exists? @config/messages-directory)
    (.mkdir (io/file @config/messages-directory))
    (spit @config/messages-filename {}))
  )

;--- Migration 4 ---

(defn migration-4-add-profiles-and-load-with-nicknames []
  (let [nicknames (read-string (slurp @config/nicknames-filename))
        profiles (apply hash-map (mapcat (fn [[k v]] (vector k {:name v})) nicknames))]
    (spit @config/profiles-filename profiles))
  )

;--- Migration 5 ---

(defn migration-5-remove-nicknames []
  (delete-file @config/nicknames-filename))

;--- Migration 6 ---

(defn migration-6-reformat-tabs []
  (if (file-exists? @config/tabs-filename)
    (let [tabs-map (read-string (slurp @config/tabs-filename))
          tabs-list (reduce (fn [l [k v]] (conj l (assoc v :name (name k)))) [] tabs-map)]
      (spit @config/tabs-list-filename
            (with-out-str (clojure.pprint/pprint tabs-list))))
    (spit @config/tabs-list-filename []))
  (delete-file @config/tabs-filename))

;--- Migration 7 break messages into daily files ---

(defn migration-7-break-messages-into-daily-files []
  (let [messages (read-string (slurp @config/messages-filename))
        partitions (data-storage/partition-messages-by-day messages)]
    (data-storage/write-messages-by-day partitions)
    (delete-file @config/messages-filename)
    )
  )

;--- Migration 8 user-configuration file ---

(defn migration-8-user-configuration []
  (spit
    @config/user-configuration-filename
    (user-configuration/validate {}))
  )

;--- Migration 9 contact lists file ---

(defn migration-9-contact-lists []
  (spit
    @config/contact-lists-filename {})
  )


;---------- The Migrations List -------

(def migrations (atom {1 initial-migration
                       2 migration-2-fix-names
                       3 migration-3-add-messages-directory
                       4 migration-4-add-profiles-and-load-with-nicknames
                       5 migration-5-remove-nicknames
                       6 migration-6-reformat-tabs
                       7 migration-7-break-messages-into-daily-files
                       8 migration-8-user-configuration
                       9 migration-9-contact-lists
                       }))

;--------------------------------------

(defn set-migration-level [n]
  (spit @migration-filename {:migration-level n}))

(defn get-migration-level []
  (if (.exists (io/file @migration-filename))
    (let [migration-level-map (read-string (slurp @migration-filename))]
      (get migration-level-map :migration-level 0))
    0))

(defn get-needed-migrations [n]
  (let [current-level (get-migration-level)]
    (if (> current-level n)
      (throw (Exception. (format "Attempting to migrate from level %d to %d."
                                 current-level n)))
      (range (inc current-level) (inc n)))))

(defn migrate-to [level]
  (let [levels (get-needed-migrations level)
        available-levels (keys @migrations)
        missing-levels (set/difference (set levels) (set available-levels))]
    (if (empty? missing-levels)
      (do
        (loop [levels levels]
          (if (empty? levels)
            nil
            (let [migration-function (get @migrations (first levels))]
              (when (some? migration-function)
                (println (format "Migrating to level %d." (first levels)))
                (migration-function)
                (recur (rest levels))))))
        (set-migration-level level))
      (throw (Exception. (format "Missing migrations %s." (vec missing-levels)))))))
