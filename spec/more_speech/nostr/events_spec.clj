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
               "tags" []
               "content" "the content"
               "sig" "dddddd"})
  (with state {:application
               {:text-event-map {}
                :chronological-text-events []}
               })
  (it "should create map of text events by id"
    (let [state (process-text-event @state @event)
          event-map (get-in state [:application :text-event-map])
          text-events (get-in state [:application :chronological-text-events])
          event (get event-map 0xdeadbeef :not-in-map)]
      (should= 1 (count event-map))
      (should= 1 (count text-events))
      (should= 0xdeadbeef (first text-events))
      (should= [0xdeadbeef] (keys event-map))
      (should= 0xdeadbeef (:id event))
      (should= 0xf00d (:pubkey event))
      (should= @now (:created-at event))
      (should= "the content" (:content event))
      (should= 0xdddddd (:sig event))
      (should= [] (:tags event))
      ))

  )
