(ns more-speech.nostr.protocol-spec
  (:require [more-speech.config :as config]
            [more-speech.db.gateway :as gateway]
            [more-speech.db.in-memory :as in-memory]
            [more-speech.mem :as mem]
            [more-speech.nostr.protocol :refer :all]
            [more-speech.nostr.util :as util]
            [more-speech.relay :as relay]
            [more-speech.websocket-relay :as ws-relay]
            [speclj.core :refer :all]))

(declare now db)
(declare until since min-time back-to relay)

(describe "protocol utilities"
  (with-stubs)
  (with now 100000)
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
    (with relay {::ws-relay/url "url" :connection :relay})
    (before (reset! mem/relays {"url" @relay}))
    (before (mem/set-mem :websocket-backlog 0))

    (it "sends subscriptions for authors"
      (with-redefs [relay/send (fn [_relay msg] (mem/update-mem :stub-send concat [msg]))
                    util/get-now (stub :get-now {:return @now})]
        (mem/set-mem :pubkey 1)
        (gateway/add-contacts @db 1 [{:pubkey 2}])
        (send-subscription @relay 1000 @now ["author1xxxxx" "author2xxxxxxxx"])
        (Thread/sleep 10)
        (should=
          #{["REQ" "ms-future" {"kinds" [0 1 2 3 4 7 9735], "since" @now, "authors" #{"author2xxx" "author1xxx"}} {"kinds" [0 1 2 3 4 7 9735], "since" @now, "#p" #{"0000000000000000000000000000000000000000000000000000000000000001" "0000000000000000000000000000000000000000000000000000000000000002"}}]
           ["REQ" "ms-past-author" {"kinds" [0 1 2 3 4 7 9735], "since" (- @now config/batch-time), "until" @now, "authors" #{"author2xxx" "author1xxx"}, "limit" config/batch-size}]
           ["REQ" "ms-past-mention" {"kinds" [0 1 2 3 4 7 9735], "since" (- @now config/batch-time), "until" @now, "#p" #{"0000000000000000000000000000000000000000000000000000000000000001" "0000000000000000000000000000000000000000000000000000000000000002"}, "limit" config/batch-size}]}
          (set (mem/get-mem :stub-send))))
      (should=
        {:eose :next-batch,
         :min-time @now,
         :last-batch-min-time @now
         :max-time 0,
         :event-counter 0,
         :back-to 1000,
         :since (- @now config/batch-time),
         :until @now,
         :filter {"kinds" [0 1 2 3 4 7 9735], "since" 1000, "until" @now, "authors" #{"author2xxx" "author1xxx"}}}
        (mem/get-mem [:active-subscriptions "url" "ms-past-author"]))
      (should=
        {:eose :next-batch,
         :min-time @now,
         :last-batch-min-time @now
         :max-time 0,
         :event-counter 0,
         :back-to 1000,
         :since (- @now config/batch-time),
         :until @now,
         :filter {"kinds" [0 1 2 3 4 7 9735], "since" 1000, "until" 100000, "#p" #{"0000000000000000000000000000000000000000000000000000000000000001" "0000000000000000000000000000000000000000000000000000000000000002"}}}
        (mem/get-mem [:active-subscriptions "url" "ms-past-mention"])))

    (it "sends subscriptions without authors"
      (with-redefs [relay/send (fn [_relay msg] (mem/update-mem :stub-send concat [msg]))
                    util/get-now (stub :get-now {:return @now})]
        (mem/set-mem :pubkey 1)
        (send-subscription @relay 1000 @now)
        (Thread/sleep 10)
        (should= #{["REQ" "ms-future"
                   {"kinds" [0 1 2 3 4 7 9735],
                    "since" @now}]
                  ["REQ" "ms-past"
                   {"kinds" [0 1 2 3 4 7 9735],
                    "since" (- @now config/batch-time),
                    "until" @now,
                    "limit" config/batch-size}]}
                 (set (mem/get-mem :stub-send)))

        (should= {:eose :next-batch,
                  :min-time @now,
                  :last-batch-min-time @now
                  :max-time 0,
                  :event-counter 0,
                  :back-to 1000,
                  :since (- @now config/batch-time),
                  :until @now,
                  :filter {"kinds" [0 1 2 3 4 7 9735], "since" 1000, "until" @now}}
                 (mem/get-mem [:active-subscriptions "url" "ms-past"]))))
    )

  (context "request batching"
    (before (reset! mem/relays {"url" {:connection :relay}}))
    (before (mem/set-mem :websocket-backlog 0))

    (it "counts events"
      (should= {:min-time 10 :max-time 10 :event-counter 1 :junk :junk}
               (count-event {:junk :junk} {"created_at" 10}))
      (should= {:min-time 10 :max-time 11 :event-counter 2 :junk :junk}
               (count-event {:min-time 10 :max-time 10 :event-counter 1 :junk :junk} {"created_at" 11}))
      (should= {:min-time 9 :max-time 10 :event-counter 3 :junk :junk}
               (count-event {:min-time 10 :max-time 10 :event-counter 2 :junk :junk} {"created_at" 9})))

    (it "starts batching subscriptions"
      (with-redefs [util/get-now (stub :get-now {:return 100000})
                    relay/send (stub :relay-send)]

        (request-batch "url" "id" 1000 {:filter :filter})
        (Thread/sleep 10)
        (should-have-invoked
          :relay-send
          {:with [:relay ["REQ" "id" {:filter :filter
                                      "since" (- 100000 config/batch-time)
                                      "until" 100000
                                      "limit" config/batch-size}]]})
        (should= {:eose :next-batch
                  :filter {:filter :filter}
                  :since (- @now config/batch-time)
                  :until @now :back-to 1000
                  :min-time @now
                  :last-batch-min-time @now
                  :max-time 0
                  :event-counter 0}
                 (mem/get-mem [:active-subscriptions "url" "id"]))))

    (context "next batch"
      (with until 100000)
      (with since (- @until config/batch-time))
      (with min-time (- @until (quot config/batch-time 2)))
      (with back-to 1000)
      (before (mem/set-mem
                [:active-subscriptions "url" "id"]
                {:eose :next-batch
                 :filter {:filter :filter}
                 :since @since
                 :until @until :back-to @back-to
                 :min-time @min-time
                 :max-time @until
                 :event-counter 0}))
      (before (mem/set-mem :websocket-backlog 0))

      (it "get next batch after average batch"
        (with-redefs [util/get-now (stub :get-now {:return @until})
                      relay/send (fn [_relay msg] (mem/update-mem :stub-send concat [msg]))]
          (let [event-counter (quot config/batch-size 2)]
            (mem/set-mem
              [:active-subscriptions "url" "id" :event-counter] event-counter)
            (request-next-batch "url" "id")
            (Thread/sleep 10)
            (should= [["CLOSE" "id"]
                      ["REQ" "id" {:filter :filter
                                   "since" @since
                                   "until" @min-time
                                   "limit" config/batch-size}]]
                     (mem/get-mem :stub-send))
            (should= {:eose :next-batch
                      :filter {:filter :filter}
                      :since @since
                      :until @min-time :back-to 1000
                      :min-time @min-time
                      :last-batch-min-time @min-time
                      :max-time 0
                      :event-counter 0
                      :batch-closed false}
                     (mem/get-mem [:active-subscriptions "url" "id"])))))

      (it "get next batch after no events in last batch"
        (with-redefs [util/get-now (stub :get-now {:return @until})
                      relay/send (fn [_relay msg] (mem/update-mem :stub-send concat [msg]))]
          (let [new-since (- @since config/batch-time)]
            (request-next-batch "url" "id")
            (Thread/sleep 10)
            (should= [["CLOSE" "id"]
                      ["REQ" "id" {:filter :filter
                                   "since" new-since
                                   "until" @since
                                   "limit" config/batch-size}]]
                     (mem/get-mem :stub-send))
            (should= {:eose :next-batch
                      :filter {:filter :filter}
                      :since new-since
                      :until @since :back-to 1000
                      :min-time @min-time
                      :last-batch-min-time @min-time
                      :max-time 0
                      :event-counter 0
                      :batch-closed false}
                     (mem/get-mem [:active-subscriptions "url" "id"])))))

      (it "get next batch when batch-time has been passed"
        (with-redefs [util/get-now (stub :get-now {:return 100000})
                      relay/send (fn [_relay msg] (mem/update-mem :stub-send concat [msg]))]
          (let [new-min-time (inc @since)]
            (mem/update-mem [:active-subscriptions "url" "id"]
                            assoc :min-time new-min-time
                            :event-counter 1)
            (let [new-since (- @since config/batch-time)]
              (request-next-batch "url" "id")
              (Thread/sleep 100)

              (should= [["CLOSE" "id"]
                        ["REQ" "id" {:filter :filter
                                     "since" new-since
                                     "until" new-min-time
                                     "limit" config/batch-size}]]
                       (mem/get-mem :stub-send))
              (should= {:eose :next-batch
                        :filter {:filter :filter}
                        :since new-since
                        :until new-min-time :back-to 1000
                        :min-time new-min-time
                        :last-batch-min-time new-min-time
                        :max-time 0
                        :event-counter 0
                        :batch-closed false}
                       (mem/get-mem [:active-subscriptions "url" "id"]))))))

      (it "get next batch when min time hasn't changed and there have been events"
        (with-redefs [util/get-now (stub :get-now {:return 100000})
                      relay/send (fn [_relay msg] (mem/update-mem :stub-send concat [msg]))]
          (let [{:keys [min-time since until]} (mem/get-mem [:active-subscriptions "url" "id"])]
            (mem/update-mem [:active-subscriptions "url" "id"]
                            assoc
                            :last-batch-min-time min-time
                            :event-counter 1)
            (request-next-batch "url" "id")
            (Thread/sleep 100)
            (should= [["CLOSE" "id"]
                      ["REQ" "id" {:filter :filter
                                   "since" since
                                   "until" (dec until)
                                   "limit" config/batch-size}]]
                     (mem/get-mem :stub-send))
            (should= {:eose :next-batch
                      :filter {:filter :filter}
                      :since since
                      :until (dec until)
                      :back-to 1000
                      :min-time min-time
                      :last-batch-min-time min-time
                      :max-time 0
                      :event-counter 0
                      :batch-closed false}
                     (mem/get-mem [:active-subscriptions "url" "id"]))))


        (it "End batch when min-time < back-to"
          (with-redefs [util/get-now (stub :get-now {:return 100000})
                        relay/send (fn [_relay msg] (mem/update-mem :stub-send concat [msg]))]
            (mem/update-mem [:active-subscriptions "url" "id"]
                            assoc :min-time (dec @back-to))
            (request-next-batch "url" "id")
            (should= [["CLOSE" "id"]] (mem/get-mem :stub-send))
            (should= nil (mem/get-mem [:active-subscriptions "url" "id"])))))
      )
    )
  )