(ns more-speech.nostr.events_spec
  (:require [speclj.core :refer :all]
            [more-speech.nostr.events :refer :all]
            [more-speech.nostr.elliptic-signature :as ecc]))

(defn hexify [n]
  (->> n (ecc/num->bytes 32) ecc/bytes->hex-string))

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
    (let [private-key (ecc/num->bytes 64 314159)
          event-state {:keys {:private-key (ecc/bytes->hex-string private-key)}}
          public-key (ecc/get-pub-key private-key)
          text "message text"
          event (compose-text-event event-state text)
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

  (it "composes a reply to a root article."
    (let [private-key (ecc/num->bytes 64 42)
          reply-to 7734
          reply-to-hex (->> reply-to (ecc/num->bytes 32) ecc/bytes->hex-string)
          event-state {:keys {:private-key (ecc/bytes->hex-string private-key)}
                       :text-event-map {reply-to {:tags []}}}
          public-key (ecc/get-pub-key private-key)
          text "message text"
          event (compose-text-event event-state text reply-to)
          {:keys [pubkey created_at kind tags content id sig]} (second event)
          now (quot (System/currentTimeMillis) 1000)]
      (should= "EVENT" (first event))
      (should= (ecc/bytes->hex-string public-key) pubkey)
      (should (<= 0 (- now created_at) 1))                  ;within one second.
      (should= 1 kind)
      (should= [[:e reply-to-hex]] tags)
      (should= text content)
      (should (ecc/do-verify (ecc/hex-string->bytes id)
                             public-key
                             (ecc/hex-string->bytes sig)))))

  (it "composes a reply to a non-root article."
    (let [private-key (ecc/num->bytes 64 42)
          reply-to-child 7734
          reply-to-hex (->> reply-to-child (ecc/num->bytes 32) ecc/bytes->hex-string)
          root 1952
          root-hex (->> root (ecc/num->bytes 32) ecc/bytes->hex-string)
          event-state {:keys {:private-key (ecc/bytes->hex-string private-key)}
                       :text-event-map {reply-to-child {:tags [[:e root-hex]]}
                                        root {:tags []}}}
          public-key (ecc/get-pub-key private-key)
          text "message text"
          event (compose-text-event event-state text reply-to-child)
          {:keys [pubkey created_at kind tags content id sig]} (second event)
          now (quot (System/currentTimeMillis) 1000)]
      (should= "EVENT" (first event))
      (should= (ecc/bytes->hex-string public-key) pubkey)
      (should (<= 0 (- now created_at) 1))                  ;within one second.
      (should= 1 kind)
      (should= [[:e root-hex] [:e reply-to-hex]] tags)
      (should= text content)
      (should (ecc/do-verify (ecc/hex-string->bytes id)
                             public-key
                             (ecc/hex-string->bytes sig)))))

  (it "composes a message with a slash."
    (let [private-key (ecc/num->bytes 64 42)
          event-state {:keys {:private-key (ecc/bytes->hex-string private-key)}}
          public-key (ecc/get-pub-key private-key)
          text "message/text"
          event (compose-text-event event-state text)
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

(describe "get references"
  (it "given no tags, finds no references"
    (let [event {:tags []}
          [root mentions referent] (get-references event)]
      (should-be-nil root)
      (should= [] mentions)
      (should-be-nil referent)))

  (it "given one tag, finds only the referent"
      (let [event {:tags [[:e (hexify 1)]]}
            [root mentions referent] (get-references event)]
        (should-be-nil root)
        (should= [] mentions)
        (should= 1 referent)))

  (it "given two tags, finds root and referent"
      (let [event {:tags [[:e (hexify 1)]
                          [:e (hexify 2)]]}
            [root mentions referent] (get-references event)]
        (should= 1 root)
        (should= [] mentions)
        (should= 2 referent)))

  (it "given n>2 tags, finds root and referent"
      (let [event {:tags [[:e (hexify 1)]
                          [:e (hexify 2)]
                          [:e (hexify 3)]
                          [:e (hexify 4)]
                          [:e (hexify 5)]]}
            [root mentions referent] (get-references event)]
        (should= 1 root)
        (should= [2 3 4] mentions)
        (should= 5 referent)))
  )

(describe "json"
  (it "does not escape slashes"
    (should= "\"/\"" (to-json "/")))

  (it "does not escape unicode"
    (should= "\"Ω\"" (to-json "\u03a9")))

  )


