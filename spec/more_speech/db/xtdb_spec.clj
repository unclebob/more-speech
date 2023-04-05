(ns more-speech.db.xtdb-spec
  (:require [more-speech.config :as config]
            [more-speech.db.gateway :as gateway]
            [more-speech.db.xtdb :as db]
            [more-speech.db.xtdb :as xtdb]
            [more-speech.nostr.util :as util]
            [more-speech.util.files :refer :all]
            [speclj.core :refer :all]))

(declare db)
(describe "xtdb gateway implementations"
  (with db (db/get-db "test-db"))
  (before-all (config/set-db! :xtdb))
  (after-all (db/stop!)
             (delete-dir "test-db"))

  (context "profiles"
    (it "adds and fetches profiles"
      (let [profile {:name "name"}]
        (gateway/add-profile @db 1N profile)
        (xtdb/sync-db @db)
        (should= profile (gateway/get-profile @db 1N)))
      (db/delete-profile @db 1N)
      (should-be-nil (gateway/get-profile @db 1N)))

    (it "gets an id from a user name"
      (gateway/add-profile @db 1N {:name "name"})
      (xtdb/sync-db @db)
      (should= 1N (gateway/get-id-from-username @db "name")))

    (it "adds a map of profiles"
      (gateway/add-profiles-map @db {1N {:name "n1"}
                                     2N {:name "n2"}})
      (should= {:name "n1"} (gateway/get-profile @db 1N))
      (should= {:name "n2"} (gateway/get-profile @db 2N)))

    (it "gets profiles after a date"
      (gateway/add-profiles-map @db {1 {:name "one" :created-at 100}
                                     2 {:name "two" :created-at 200}
                                     3 {:name "three" :created-at 300}})
      (should= #{[2 "two"]
                 [3 "three"]} (set (gateway/get-profiles-after @db 150))))
    )

  (context "events"
    (it "adds and fetches events"
      (let [event {:id 1N :content "content"}]
        (gateway/add-event @db event)
        (xtdb/sync-db @db)
        (should= event (gateway/get-event @db 1N)))
      (db/delete-event @db 1N)
      (should-be-nil (gateway/get-event @db 1N)))

    (it "checks whether events exist"
      (gateway/add-event @db {:id 1N :content "blah"})
      (xtdb/sync-db @db)
      (should (gateway/event-exists? @db 1N))
      (should-not (gateway/event-exists? @db 2N))
      (db/delete-event @db 1N)
      (should-not (gateway/event-exists? @db 1N)))

    (it "updates events as read"
      (gateway/add-event @db {:id 1N :content "blah"})
      (gateway/update-event-as-read @db 1N)
      (should= {:id 1N :content "blah" :read true}
               (gateway/get-event @db 1N))
      (db/delete-event @db 1N))

    (it "adds relays to events"
      (gateway/add-event @db {:id 1N :content "blah"})
      (gateway/add-relays-to-event @db 1N ["r1" "r2"])
      (should= {:id 1N :content "blah" :relays #{"r1" "r2"}}
               (gateway/get-event @db 1N))
      (db/delete-event @db 1N))

    (it "adds a reference to an event"
      (gateway/add-event @db {:id 1N :content "blah"})
      (gateway/add-reference-to-event @db 1N "reference")
      (should= {:id 1N :content "blah" :references ["reference"]}
               (gateway/get-event @db 1N))
      (db/delete-event @db 1N))

    (it "adds a batch of events"
      (gateway/add-events @db [{:id 1N :content 1}
                               {:id 2N :content 2}])
      (should= {:id 1N :content 1} (gateway/get-event @db 1))
      (should= {:id 2N :content 2} (gateway/get-event @db 2)))

    (it "finds events after a certain time"
      (gateway/add-events @db [{:id 1N :created-at 1}
                               {:id 2N :created-at 10}
                               {:id 3N :created-at 11}])
      (should= #{2 3}
               (set (gateway/get-event-ids-since @db 10))))

    (it "finds read events after a certain time"
      (gateway/add-events @db [{:id 1N :created-at 1}
                               {:id 2N :created-at 10 :read true}
                               {:id 3N :created-at 11}])
      (should= [2]
               (gateway/get-ids-of-read-events-since @db 10)))

    (it "gets events by author since date"
      (gateway/add-events @db [{:id 1N :pubkey 100N :created-at 0}
                               {:id 2N :pubkey 100N :created-at 10}
                               {:id 3N :pubkey 100N :created-at 11}
                               {:id 4N :pubkey 200N :created-at 12}])
      (should= #{2 3} (set (gateway/get-ids-by-author-since @db 100N 9))))

    (it "gets events cited by others"
      (gateway/add-events @db [{:id 1N :created-at 0 :tags []}
                               {:id 2N :created-at 10 :tags [[:a 1]]}
                               {:id 3N :created-at 20 :tags (list [:b (util/hexify 1)] [:e (util/hexify 7)])}
                               {:id 4N :created-at 20 :tags [[:b (util/hexify 2)] [:e (util/hexify 1)]]}
                               {:id 5N :created-at 30 :tags []}])
      (should= #{3N 4N} (set (gateway/get-ids-that-cite-since @db 1 11))))

    (it "gets events after a date"
        (gateway/add-event @db {:id 1 :pubkey 1001 :created-at 100})
        (gateway/add-event @db {:id 2 :pubkey 1002 :created-at 200})
        (gateway/add-event @db {:id 3 :pubkey 1003 :created-at 300})
        (gateway/add-event @db {:id 4 :pubkey 1004 :created-at 400})
        (xtdb/sync-db @db)
        (should= #{1003 1004} (gateway/get-some-recent-event-authors @db 200)))
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
        (should (every? #(= (type 1N) (type %)) pubkeys)))))

  (context "reactions"
    (it "adds reactions to events"
      (gateway/add-event @db {:id 1})
      (gateway/add-reaction @db 1 2 "!")
      (should= {:id 1 :reactions #{[2 "!"]}}
               (gateway/get-event @db 1))))
  )


