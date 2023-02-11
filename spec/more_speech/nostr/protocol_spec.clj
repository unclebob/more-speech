(ns more-speech.nostr.protocol-spec
  (:require [speclj.core :refer :all]
            [more-speech.nostr.protocol :refer :all]
            [more-speech.nostr.util :as util]))

(declare now)
(describe "protocol utilities"
  (with-stubs)
  (with now 100)
  (it "increments relay retries on un-retried relays"
    (with-redefs [util/get-now (stub :get-now {:return @now})]
      (should= {"relay" {:retries 1, :retry-time @now}}
               (increment-relay-retry {"relay" {}} "relay"))))

  (it "increments relay retries on retried relays"
      (with-redefs [util/get-now (stub :get-now {:return @now})]
        (should= {"relay" {:retries 3, :retry-time @now}}
                 (increment-relay-retry {"relay" {:retries 2 :retry-time (dec @now)}} "relay"))))

  (it "resets relay retries on relays that haven't been retried in over an hour"
      (with-redefs [util/get-now (stub :get-now {:return @now})]
        (should= {"relay" {:retries 1, :retry-time @now}}
                 (increment-relay-retry {"relay" {:retries 10 :retry-time (- @now 3600001)}} "relay"))))
  )