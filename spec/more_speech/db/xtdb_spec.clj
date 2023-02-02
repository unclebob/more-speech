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
        (should= profile (gateway/get-profile @db 1)))
      (db/delete-profile @db 1)
      (should-be-nil (gateway/get-profile @db 1)))

    (it "gets and id from a user name"
      (gateway/add-profile @db 1 {:name "name"})
      (should= 1 (gateway/get-id-from-username @db "name"))))

  (context "events"
    (it "adds and fetches events"
      (let [event {:content "content"}]
        (gateway/add-event @db 1 event)
        (should= event (gateway/get-event @db 1)))
      (db/delete-event @db 1)
      (should-be-nil (gateway/get-event @db 1)))

    (it "checks whether events exist"
      (gateway/add-event @db 1 {:content "blah"})
      (should (gateway/event-exists? @db 1))
      (should-not (gateway/event-exists? @db 2))
      (db/delete-event @db 1)
      (should-not (gateway/event-exists? @db 1)))

    (it "updates events as read"
      (gateway/add-event @db 1 {:content "blah"})
      (gateway/update-event-as-read @db 1)
      (should= {:content "blah" :read true}
               (gateway/get-event @db 1))
      (db/delete-event @db 1))

    (it "adds relays to events"
      (gateway/add-event @db 1 {:content "blah"})
      (gateway/add-relays-to-event @db 1 ["r1" "r2"])
      (should= {:content "blah" :relays #{"r1" "r2"}}
               (gateway/get-event @db 1))
      (db/delete-event @db 1))

    (it "adds a reference to an event"
      (gateway/add-event @db 1 {:content "blah"})
      (gateway/add-reference-to-event @db 1 "reference")
      (should= {:content "blah" :references ["reference"]}
               (gateway/get-event @db 1))
      (db/delete-event @db 1))

    (it "loads a batch of events"
      (xtdb/add-events @db [{:id 1 :content 1}
                            {:id 2 :content 2}])
      (should= {:id 1 :content 1} (gateway/get-event @db 1))
      (should= {:id 2 :content 2} (gateway/get-event @db 2)))

    (it "finds events after a certain time"
      (xtdb/add-events @db [{:id 1 :created-at 1}
                            {:id 2 :created-at 10}
                            {:id 3 :created-at 11}])
      (should= #{2 3}
               (set (gateway/get-event-ids-since @db 10))))
      )

    (context "contacts"
      (it "adds and fetches contacts"
        (gateway/add-contacts @db 1 {:name "contact"})
        (should= {:name "contact"} (gateway/get-contacts @db 1))
        (db/delete-contacts @db 1)
        (should-be-nil (gateway/get-contacts @db 1)))

      )
    )


