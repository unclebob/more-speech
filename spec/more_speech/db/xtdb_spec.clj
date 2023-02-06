(ns more-speech.db.xtdb-spec
  (:require [speclj.core :refer :all]
            [more-speech.db.gateway :as gateway]
            [more-speech.db.xtdb :as db]
            [more-speech.config :as config]
            [more-speech.util.files :refer :all]
            [more-speech.db.xtdb :as xtdb]))

(declare db)
(describe "xtdb gateway implementations"
  (with db (db/get-db "test-db"))
  (before-all (config/set-db! :xtdb))
  (after-all (db/stop!)
             (delete-dir "test-db"))

  (context "profiles"
    (it "adds and fetches profiles"
      (let [profile {:name "name"}]
        (gateway/add-profile @db 1 profile)
        (xtdb/sync-db @db)
        (should= profile (gateway/get-profile @db 1)))
      (db/delete-profile @db 1)
      (should-be-nil (gateway/get-profile @db 1)))

    (it "gets an id from a user name"
      (gateway/add-profile @db 1 {:name "name"})
      (xtdb/sync-db @db)
      (should= 1 (gateway/get-id-from-username @db "name"))))

  (it "adds a map of profiles"
    (gateway/add-profiles-map @db {1 {:name "n1"}
                                2 {:name "n2"}})
    (should= {:name "n1"} (gateway/get-profile @db 1))
    (should= {:name "n2"} (gateway/get-profile @db 2)))

  (context "events"
    (it "adds and fetches events"
      (let [event {:id 1 :content "content"}]
        (gateway/add-event @db event)
        (xtdb/sync-db @db)
        (should= event (gateway/get-event @db 1)))
      (db/delete-event @db 1)
      (should-be-nil (gateway/get-event @db 1)))

    (it "checks whether events exist"
      (gateway/add-event @db {:id 1 :content "blah"})
      (xtdb/sync-db @db)
      (should (gateway/event-exists? @db 1))
      (should-not (gateway/event-exists? @db 2))
      (db/delete-event @db 1)
      (should-not (gateway/event-exists? @db 1)))

    (it "updates events as read"
      (gateway/add-event @db {:id 1 :content "blah"})
      (gateway/update-event-as-read @db 1)
      (should= {:id 1N :content "blah" :read true}
               (gateway/get-event @db 1))
      (db/delete-event @db 1))

    (it "adds relays to events"
      (gateway/add-event @db {:id 1 :content "blah"})
      (gateway/add-relays-to-event @db 1 ["r1" "r2"])
      (should= {:id 1N :content "blah" :relays #{"r1" "r2"}}
               (gateway/get-event @db 1))
      (db/delete-event @db 1))

    (it "adds a reference to an event"
      (gateway/add-event @db {:id 1 :content "blah"})
      (gateway/add-reference-to-event @db 1 "reference")
      (should= {:id 1 :content "blah" :references ["reference"]}
               (gateway/get-event @db 1))
      (db/delete-event @db 1))

    (it "adds a batch of events"
      (gateway/add-events @db [{:id 1 :content 1}
                            {:id 2 :content 2}])
      (should= {:id 1 :content 1} (gateway/get-event @db 1))
      (should= {:id 2 :content 2} (gateway/get-event @db 2)))

    (it "finds events after a certain time"
      (gateway/add-events @db [{:id 1 :created-at 1}
                            {:id 2 :created-at 10}
                            {:id 3 :created-at 11}])
      (should= #{2 3}
               (set (gateway/get-event-ids-since @db 10))))
    )

  (context "contacts"
    (it "adds and fetches contacts"
      (gateway/add-contacts @db 1 {:name "contact"})
      (xtdb/sync-db @db)
      (should= {:name "contact"} (gateway/get-contacts @db 1))
      (db/delete-contacts @db 1)
      (should-be-nil (gateway/get-contacts @db 1)))

    (it "adds a batch of contacts"
      (gateway/add-contacts-map @db {1 [{:pubkey 99} {:pubkey 98}]
                                  2 [{:pubkey 97} {:pubkey 96}]})
      (let [contacts1 (gateway/get-contacts @db 1)
            contacts2 (gateway/get-contacts @db 2)
            pubkeys1 (map :pubkey contacts1)
            pubkeys2 (map :pubkey contacts2)
            pubkeys (concat pubkeys1 pubkeys2)]
        (should= [{:pubkey 99} {:pubkey 98}]
                 contacts1)
        (should= [{:pubkey 97} {:pubkey 96}]
                 contacts2)
        (should (every? #(= (type 1N) (type %)) pubkeys))
        )
      )

    )
  )


