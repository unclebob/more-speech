(ns more-speech.migrator
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [more-speech.config :refer [migration-filename]]
            [more-speech.config :as config]
            [more-speech.nostr.util :as util]
            [more-speech.nostr.elliptic-signature :as ecc]
            [more-speech.nostr.events :as events])
  (:import (java.security SecureRandom)))

(defn file-exists? [fname]
  (.exists (io/file fname)))

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

(defn migration-2-fix-names []
  (let [nicknames (read-string (slurp @config/nicknames-filename))
        fixed-nicknames (apply hash-map (flatten (map (fn [v] (vector(first v) (events/fix-name (second v)))) nicknames)))]
    (spit @config/nicknames-filename fixed-nicknames)
    ))


(def migrations (atom {1 initial-migration
                       2 migration-2-fix-names}))

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
