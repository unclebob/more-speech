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
  )
