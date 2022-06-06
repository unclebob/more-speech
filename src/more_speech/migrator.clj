(ns more-speech.migrator
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(def migration-filename (atom "private/migration"))

(def migrations (atom {}))

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