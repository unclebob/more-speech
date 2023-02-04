(ns more-speech.migrator
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [more-speech.config :refer [migration-filename]]
            [more-speech.config :as config]
            [more-speech.nostr
             [util :as util]
             [elliptic-signature :as ecc]
             [event-handlers :as handlers]]
            [more-speech.data-storage :as data-storage]
            [more-speech.user-configuration :as user-configuration]
            [more-speech.db.gateway :as gateway]
            [more-speech.util.files :refer :all]))

;---The Migrations

(defn initial-migration []
  (when-not (file-exists? @config/private-directory)
    (.mkdir (io/file @config/private-directory)))
  (when-not (file-exists? @config/keys-filename)
    (let [private-key (util/make-private-key)
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
        fixed-nicknames (apply hash-map (flatten (map (fn [v] (vector (first v) (handlers/fix-name (second v)))) nicknames)))]
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

;--- Migration 10 XTDB database conversion

(defn migrate-id-map-file [filename add-f]
  (when (file-exists? filename)
    (let [id-map (read-string (slurp filename))
          n-ids (count (keys id-map))]
      (prn 'adding n-ids 'records 'from filename)
      (add-f (config/get-db) id-map)
      (rename-file filename (str filename ".migrated")))))

(defn migration-10-load-profiles []
  (migrate-id-map-file @config/profiles-filename gateway/add-profiles-map))

(defn migration-10-load-contacts []
  (migrate-id-map-file @config/contact-lists-filename gateway/add-contacts-map))

(defn is-message-file? [file-name]
  (re-matches #"\d+\-\d+\w+\d+" file-name))

(defn load-event-file [file-name]
  (let [events (read-string (slurp file-name))
        total-events (count events)]
    (prn 'loading total-events 'from file-name)
    (gateway/add-events (config/get-db) events)))

(defn migration-10-load-events []
  (when (file-exists? @config/messages-directory)
    (let [message-directory (io/file @config/messages-directory)
          files (.listFiles message-directory)
          file-names (for [file files] (.getName file))
          file-names (filter is-message-file? file-names)]
      (loop [file-names file-names]
        (if (empty? file-names)
          (println "Done loading event files.\n")
          (let [file-name (first file-names)
                file-path (str @config/messages-directory "/" file-name)]
            (load-event-file file-path)
            (rename-file file-path (str file-path ".migrated"))
            (recur (rest file-names)))))
      (rename-file @config/messages-directory (str @config/messages-directory ".migrated")))))

(defn migration-10-XTDB-converstion []
  (migration-10-load-profiles)
  (migration-10-load-contacts)
  (migration-10-load-events))




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
                       10 migration-10-XTDB-converstion
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


