(ns more-speech.nostr.events_spec
  (:require [speclj.core :refer :all]
            [more-speech.nostr.events :refer :all]
            [more-speech.nostr.elliptic-signature :as ecc]))

(defrecord event-handler-dummy []
  event-handler
  (handle-text-event [_ _event-id])
  )

(declare now event state)
(describe "Processing Text events (Kind 1)"
  (with now (int (/ (System/currentTimeMillis) 1000)))
  (with event {"id" "deadbeef"
               "pubkey" "f00d"
               "created_at" @now
               "kind" 1
               "tags" [["p" "0001" "someurl"]
                       ["e" "0002" "anotherurl"]]
               "content" "the content"
               "sig" "dddddd"})
  (with state
        {:text-event-map {}
         :chronological-text-events (make-chronological-text-events)
         :event-handler (->event-handler-dummy)
         }
        )
  (it "creates the map of text events by id"
    (let [state (process-text-event @state @event)
          event-map (get-in state [:text-event-map])
          text-events (get-in state [:chronological-text-events])
          event (get event-map 0xdeadbeef :not-in-map)]
      (should= 1 (count event-map))
      (should= 1 (count text-events))
      (should= [0xdeadbeef @now] (first text-events))
      (should= [0xdeadbeef] (keys event-map))
      (should= 0xdeadbeef (:id event))
      (should= 0xf00d (:pubkey event))
      (should= @now (:created-at event))
      (should= "the content" (:content event))
      (should= 0xdddddd (:sig event))
      (should= [[:p "0001" "someurl"]
                [:e "0002" "anotherurl"]] (:tags event))))

  (it "adds references to tagged articles."
    (let [state (assoc-in @state
                          [:text-event-map 2]
                          {:id 2})
          state (process-references state (translate-text-event @event))
          text-event-map (get-in state [:text-event-map])
          article (get text-event-map 2)]
      (should= [0xdeadbeef] (:references article)))
    )

  (context "sorted set for handling events"
    (it "adds one element"
      (let [state (add-event @state {:id 10 :created-at 0})]
        (should= #{[10 0]} (get-in state [:chronological-text-events]))
        (should= {10 {:id 10 :created-at 0}} (get-in state [:text-event-map]))))

    (it "adds two elements in chronological order, should be reversed"
      (let [state (add-event @state {:id 10 :created-at 0})
            state (add-event state {:id 20 :created-at 1})
            ]
        (should= [[20 1] [10 0]] (seq (get-in state [:chronological-text-events])))
        (should= {10 {:id 10 :created-at 0}
                  20 {:id 20 :created-at 1}} (get-in state [:text-event-map])))
      )
    (it "adds two elements in reverse chronological order, should remain."
      (let [state (add-event @state {:id 10 :created-at 1})
            state (add-event state {:id 20 :created-at 0})
            ]
        (should= [[10 1] [20 0]] (seq (get-in state [:chronological-text-events])))
        (should= {10 {:id 10 :created-at 1}
                  20 {:id 20 :created-at 0}} (get-in state [:text-event-map])))
      )

    (it "adds two elements with equal ids"
      (let [state (add-event @state {:id 10 :created-at 1})
            state (add-event state {:id 10 :created-at 0})
            event-map (get-in state [:text-event-map])]
        (should= [[10 1]] (seq (get-in state [:chronological-text-events])))
        (should= 1 (count event-map))
        )
      )
    )
  )

(describe "Composing outgoing events"
  (it "composes an original message."
    (let [private-key (ecc/num->bytes 64 42)
          public-key (ecc/get-pub-key private-key)
          text "message text"
          event (compose-text-event private-key text)
          {:keys [pubkey created_at kind tags content id sig]} (second event)
          now (quot (System/currentTimeMillis) 1000)]
      (should= "EVENT" (first event))
      (should= (ecc/bytes->hex-string public-key) pubkey)
      (should (<= 0 (- now created_at) 1))                  ;within one second.
      (should= 1 kind)
      (should= [] tags)
      (should= text content)
      (should (ecc/do-verify (ecc/hex-string->bytes id)
                             public-key
                             (ecc/hex-string->bytes sig)))))

  (it "composes a reply."
    (let [private-key (ecc/num->bytes 64 42)
          public-key (ecc/get-pub-key private-key)
          reply-to (ecc/num->bytes 32 7734)
          text "message text"
          event (compose-text-event private-key text reply-to)
          {:keys [pubkey created_at kind tags content id sig]} (second event)
          now (quot (System/currentTimeMillis) 1000)]
      (should= "EVENT" (first event))
      (should= (ecc/bytes->hex-string public-key) pubkey)
      (should (<= 0 (- now created_at) 1))                  ;within one second.
      (should= 1 kind)
      (should= [[:e (ecc/bytes->hex-string reply-to)]] tags)
      (should= text content)
      (should (ecc/do-verify (ecc/hex-string->bytes id)
                             public-key
                             (ecc/hex-string->bytes sig)))))

  (it "composes a message with a slash."
    (let [private-key (ecc/num->bytes 64 42)
          public-key (ecc/get-pub-key private-key)
          reply-to nil
          text "message/text"
          event (compose-text-event private-key text reply-to)
          {:keys [pubkey created_at kind tags content id sig]} (second event)
          now (quot (System/currentTimeMillis) 1000)]
      (should= "EVENT" (first event))
      (should= (ecc/bytes->hex-string public-key) pubkey)
      (should (<= 0 (- now created_at) 1))                  ;within one second.
      (should= 1 kind)
      (should= [] tags)
      (should= text content)
      (should (ecc/do-verify (ecc/hex-string->bytes id)
                             public-key
                             (ecc/hex-string->bytes sig)))))
  )

(describe "json"
  (it "does not escape slashes"
    (should= "\"/\"" (to-json "/")))

  (it "does not escape unicode"
    (should= "\"Ω\"" (to-json "\u03a9")))

  )


