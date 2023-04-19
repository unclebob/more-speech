(ns more-speech.nostr.protocol-spec
  (:require [speclj.core :refer :all]
            [more-speech.mem :as mem]
            [more-speech.nostr.protocol :refer :all]
            [more-speech.nostr.util :as util]
            [more-speech.relay :as relay]
            [more-speech.config :as config]
            [more-speech.db.in-memory :as in-memory]
            [more-speech.db.gateway :as gateway]))

(declare now db)
(describe "protocol utilities"
  (with-stubs)
  (with now 100)
  (with db (in-memory/get-db))
  (before-all (config/set-db! :in-memory))
  (before (in-memory/clear-db @db))
  (before (mem/clear-mem))
  (it "increments relay retries on un-retried relays"
    (with-redefs [util/get-now-ms (stub :get-now {:return @now})]
      (should= {"relay" {:retries 1, :retry-time @now}}
               (increment-relay-retry {"relay" {}} "relay"))))

  (it "increments relay retries on retried relays"
    (with-redefs [util/get-now-ms (stub :get-now {:return @now})]
      (should= {"relay" {:retries 3, :retry-time @now}}
               (increment-relay-retry {"relay" {:retries 2 :retry-time (dec @now)}} "relay"))))

  (it "resets relay retries on relays that haven't been retried in over an hour"
    (with-redefs [util/get-now-ms (stub :get-now {:return @now})]
      (should= {"relay" {:retries 1, :retry-time @now}}
               (increment-relay-retry {"relay" {:retries 10 :retry-time (- @now 3600001)}} "relay"))))

  (context "relay subscription event times"
    (it "adds a time to a non existent pair"
      (should= [99 99] (add-event-time nil 99)))

    (it "should add earliest time"
      (should= [80 90] (add-event-time [90 90] 80)))

    (it "should add latest time"
      (should= [90 100] (add-event-time [90 90] 100)))
    )

  (context "sending subscriptions"
    (it "sends subscriptions for authors"
      (with-redefs [relay/send (stub :send)]
        (mem/set-mem :pubkey 1)
        (gateway/add-contacts @db 1 [{:pubkey 2}])
        (send-subscription :relay 0 100 ["author1xxxxx" "author2xxxxxxxx"])
        (should-have-invoked
          :send {:with [:relay ["REQ" "ms-past"
                                {"kinds" [0 1 2 3 4 7 9735], "since" 0
                                 "until" 100
                                 "authors" #{"author1xxx" "author2xxx"}}
                                {"kinds" [0 1 2 3 4 7 9735], "since" 0
                                 "until" 100
                                 "#p" #{"0000000000000000000000000000000000000000000000000000000000000001"
                                        "0000000000000000000000000000000000000000000000000000000000000002"}}]]})
        (should-have-invoked
          :send {:with [:relay ["REQ" "ms-future"
                                {"kinds" [0 1 2 3 4 7 9735], "since" 100
                                 "authors" #{"author1xxx" "author2xxx"}}
                                {"kinds" [0 1 2 3 4 7 9735], "since" 100
                                 "#p" #{"0000000000000000000000000000000000000000000000000000000000000001"
                                        "0000000000000000000000000000000000000000000000000000000000000002"}}]]})))

    (it "sends subscriptions without authors"
      (with-redefs [relay/send (stub :send)]
        (mem/set-mem :pubkey 1)
        (send-subscription :relay 0 100)
        (should-have-invoked :send {:with [:relay
                                           ["REQ" "ms-past"
                                            {"kinds" [0 1 2 3 4 7 9735], "since" 0
                                             "until" 100}]]})
        (should-have-invoked :send {:with [:relay ["REQ" "ms-future"
                                                   {"kinds" [0 1 2 3 4 7 9735], "since" 100}]]})))
    )
  )