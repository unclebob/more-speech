(ns more-speech.nostr.protocol-spec
  (:require [speclj.core :refer :all]
            [more-speech.nostr.protocol :refer :all]
            [more-speech.nostr.util :as util]
            [more-speech.relay :as relay]))

(declare now)
(describe "protocol utilities"
  (with-stubs)
  (with now 100)
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
    (it "has some comments to be removed..."
      (pending "delete them."))
    (it "sends subscriptions for authors"
      (with-redefs [relay/send (stub :send)]
        (send-subscription :relay 0 100 [:author1 :author2])
        (should-have-invoked
          :send {:with [:relay ["REQ" "ms-past"
                                {"since" 0
                                 "until" 100
                                 "authors" #{:author1 :author2}}
                                #_{"since" 0
                                 "until" 100
                                 "#p" #{:author1 :author2}}]]})
        (should-have-invoked
          :send {:with [:relay ["REQ" "ms-future"
                                {"since" 100
                                 "authors" #{:author1 :author2}}
                                #_{"since" 100
                                 "#p" #{:author1 :author2}}]]})))

    (it "sends subscriptions without authors"
      (with-redefs [relay/send (stub :send)]
        (send-subscription :relay 0 100)
        (should-have-invoked :send {:with [:relay
                                           ["REQ" "ms-past"
                                            {"since" 0
                                             "until" 100}]]})
        (should-have-invoked :send {:with [:relay ["REQ" "ms-future"
                                                   {"since" 100}]]})))
    )
  )