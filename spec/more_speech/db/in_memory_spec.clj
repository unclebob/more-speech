(ns more-speech.db.in-memory-spec
  (:require [more-speech.db.gateway :as gateway]
            [more-speech.db.in-memory :as in-memory]
            [speclj.core :refer :all]))

(declare db)
(describe "in-memory database"
  (with db (in-memory/get-db))
  (before (in-memory/clear-db @db))

  (it "checks for existing event"
    (gateway/add-event @db {:id 1 :content "event"})
    (should (gateway/event-exists? @db 1))
    (should-not (gateway/event-exists? @db 2)))

  (it "gets events by author since date"
    (gateway/add-events @db [{:id 1 :pubkey 100 :created-at 0}
                             {:id 2 :pubkey 100 :created-at 10}
                             {:id 3 :pubkey 100 :created-at 11}
                             {:id 4 :pubkey 200 :created-at 12}])
    (should= [2 3] (gateway/get-ids-by-author-since @db 100 9)))

  (it "gets events cited by others"
    (gateway/add-events @db [{:id 1 :created-at 0 :tags []}
                             {:id 2 :created-at 10 :tags [[:a 1]]}
                             {:id 3 :created-at 20 :tags [[:b 1] [:e 7]]}
                             {:id 4 :created-at 20 :tags [[:b 2] [:e 1]]}
                             {:id 5 :created-at 30 :tags []}])
    (should= #{3 4} (set (gateway/get-ids-that-cite-since @db 1 11))))

  (it "gets profiles after a date"
    (gateway/add-profiles-map @db {1 {:name "one" :created-at 100}
                                   2 {:name "two" :created-at 200}
                                   3 {:name "three" :created-at 300}})
    (should= #{[2 "two"]
               [3 "three"]} (set (gateway/get-profiles-after @db 150))))

  (it "gets events after a date"
    (gateway/add-event @db {:id 1 :pubkey 1001 :created-at 100})
    (gateway/add-event @db {:id 2 :pubkey 1002 :created-at 200})
    (gateway/add-event @db {:id 3 :pubkey 1003 :created-at 300})
    (gateway/add-event @db {:id 4 :pubkey 1004 :created-at 400})

    (should= #{1003 1004} (gateway/get-some-recent-event-authors @db 200))

    )

  )
