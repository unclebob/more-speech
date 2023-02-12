(ns more-speech.nostr.event-handlers-spec
  (:require [speclj.core :refer :all]
            [more-speech.nostr.event-handlers :as handlers]
            [more-speech.db.gateway :as gateway]
            [more-speech.db.in-memory :as in-memory]
            [more-speech.config :as config]))

(declare db)
(describe "event-handlers"
  (with-stubs)
  (with db (in-memory/get-db))
  (before-all (config/set-db! :in-memory))
  (before (in-memory/clear-db @db))

  (context "Kind 0 name event"
    (it "Does not add a suffix to names that don't exist."
      (gateway/add-profile @db 1 {:name "name-exists"})
      (should= "new-name" (handlers/add-suffix-for-duplicate 2 "new-name")))

    (it "Adds a single digit suffix when name already exists."
      (gateway/add-profile @db 1 {:name "name-exists"})
      (let [new-name (handlers/add-suffix-for-duplicate 2 "name-exists")]
        (should= "ok"
                 (if (re-matches #"name-exists\d" new-name)
                   "ok" new-name))))

    (it "Adds a two (or more) digit suffix when it collides with a one digit suffix."
         (gateway/add-profile @db 1 {:name "name-exists"})
         (doseq [n (range 1 10)]
                 (gateway/add-profile @db (+ 10 n) {:name (str "name-exists" n)}))
         (let [new-name (handlers/add-suffix-for-duplicate 2 "name-exists")]
           (should= "ok"
                    (if (re-matches #"name-exists\d\d+" new-name)
                      "ok" new-name))))

    (it "Adds a three (or more) digit suffix when it collides with a one or two digit suffix."
             (gateway/add-profile @db 1 {:name "name-exists"})
             (doseq [n (range 1 100)]
                     (gateway/add-profile @db (+ 10 n) {:name (str "name-exists" n)}))
             (let [new-name (handlers/add-suffix-for-duplicate 2 "name-exists")]
               (should= "ok"
                        (if (re-matches #"name-exists\d\d\d+" new-name)
                          "ok" new-name))))

    )
  )
