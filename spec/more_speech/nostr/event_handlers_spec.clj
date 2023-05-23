(ns more-speech.nostr.event-handlers-spec
  (:require
    [more-speech.nostr.util :as util]
    [speclj.core :refer :all]
    [more-speech.nostr.event-dispatcher :as handlers]
    [more-speech.db.gateway :as gateway]
    [more-speech.db.in-memory :as in-memory]
    [more-speech.config :as config]
    [more-speech.mem :refer :all]))

(declare db)
(describe "event-handlers"
  (with-stubs)
  (with db (in-memory/get-db))
  (before-all (config/set-db! :in-memory))
  (before (in-memory/clear-db @db))
  (before (clear-mem))

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

  (context "handling duplicate events"
    (it "validates a non-duplicate"
      (let [event {:id 1 :content "content" :kind 1}]
        (with-redefs
          [handlers/handle-text-event (stub :handle-text-event)
           handlers/process-event (stub :process-event)
           util/translate-event (stub :translate-event {:return event})
           handlers/decrypt-dm-event (stub :decrypt-dm-event {:return event})
           handlers/compute-id (stub :compute-id {:return 1})]
          (set-mem [:processed-event-ids] {})
          (handlers/validate-and-process-event
            :url [:type :subscription-id event]))

        (should-have-invoked :handle-text-event)
        (should= {1 #{:url}} (get-mem [:processed-event-ids]))
        (should= 1 (get-mem [:event-counter :kinds 1]))))

    (it "validates a duplicate"
          (let [event {:id 1 :content "content" :kind 1}]
            (with-redefs
              [handlers/handle-text-event (stub :handle-text-event)
               handlers/process-event (stub :process-event)
               util/translate-event (stub :translate-event {:return event})
               handlers/decrypt-dm-event (stub :decrypt-dm-event {:return event})
               handlers/compute-id (stub :compute-id {:return 1})]
              (set-mem [:processed-event-ids] {1 #{:first-url}})
              (handlers/validate-and-process-event
                :url [:type :subscription-id event]))

            (should-not-have-invoked :handle-text-event)
            (should= {1 #{:first-url :url}} (get-mem [:processed-event-ids]))
            (should= 1 (get-mem [:event-counter :kinds 1]))))
    )
  )
