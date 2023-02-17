(ns more-speech.db.in-memory-spec
  (:require [speclj.core :refer :all]
            [more-speech.db.gateway :as gateway]
            [more-speech.db.in-memory :as in-memory]))

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
  )
