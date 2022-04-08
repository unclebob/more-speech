(ns more-speech.nostr.events_spec
  (:require [speclj.core :refer :all]
            [more-speech.nostr.events :refer :all]))

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
  (with state {:application
               {:text-event-map {}
                :chronological-text-events (make-chronological-text-events)
                }
               })
  (it "creates the map of text events by id"
    (let [state (process-text-event @state @event)
          event-map (get-in state [:application :text-event-map])
          text-events (get-in state [:application :chronological-text-events])
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
                          [:application :text-event-map 2]
                          {:id 2})
          state (process-references state (translate-text-event @event))
          text-event-map (get-in state [:application :text-event-map])
          article (get text-event-map 2)]
      (should= [0xdeadbeef] (:references article)))
    )

  (context "sorted set for handling events"
    (it "adds one element"
      (let [state (add-event @state {:id 10 :created-at 0})]
        (should= #{[10 0]} (get-in state [:application :chronological-text-events]))
        (should= {10 {:id 10 :created-at 0}} (get-in state [:application :text-event-map]))))

    (it "adds two elements in chronological order, should be reversed"
      (let [state (add-event @state {:id 10 :created-at 0})
            state (add-event state {:id 20 :created-at 1})
            ]
        (should= [[20 1] [10 0]] (seq (get-in state [:application :chronological-text-events])))
        (should= {10 {:id 10 :created-at 0}
                  20 {:id 20 :created-at 1}} (get-in state [:application :text-event-map])))
      )
    (it "adds two elements in reverse chronological order, should remain."
      (let [state (add-event @state {:id 10 :created-at 1})
            state (add-event state {:id 20 :created-at 0})
            ]
        (should= [[10 1] [20 0]] (seq (get-in state [:application :chronological-text-events])))
        (should= {10 {:id 10 :created-at 1}
                  20 {:id 20 :created-at 0}} (get-in state [:application :text-event-map])))
      )

    (it "adds two elements with equal ids"
      (let [state (add-event @state {:id 10 :created-at 1})
            state (add-event state {:id 10 :created-at 0})
            event-map (get-in state [:application :text-event-map])]
        (should= [[10 1]] (seq (get-in state [:application :chronological-text-events])))
        (should= 1 (count event-map))
        )
      )
    )
  )


