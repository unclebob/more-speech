(ns more-speech.ui.article-window-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.article-window :refer :all]
            [more-speech.ui.widget :refer [widget setup-widget]]))

(describe "article window"
  (context "setup"
    (it "has the components"
      (let [state {}
            article-window (map->article-window {:x 0 :y 0 :h 100 :w 100})
            article-window (setup-widget article-window state)]
        (should (satisfies? widget (:page-up article-window)))
        (should (satisfies? widget (:page-down article-window)))
        ))))

(describe "threading of events"
  (it "does not thread if open events that have no references"
    (let [event1 {:id 1}
          event2 {:id 2}
          event-map {1 event1
                     2 event2}
          events [1 2]
          open-events #{1 2}
          threaded-events (thread-events events event-map open-events)]
      (should= [1 2] (map :id threaded-events))
      (should= [0 0] (map :indent threaded-events)))
    )

  (it "does not thread events that have references but are not open"
      (let [event1 {:id 1 :references [3]}
            event2 {:id 2}
            event3 {:id 3}
            event-map {1 event1
                       2 event2
                       3 event3}
            events [1 2 3]
            open-events #{}
            threaded-events (thread-events events event-map open-events)]
        (should= [1 2 3] (map :id threaded-events))
        (should= [0 0 0] (map :indent threaded-events))))

  (it "threads events that have references and are open"
        (let [event1 {:id 1 :references [3]}
              event2 {:id 2}
              event3 {:id 3}
              event-map {1 event1
                         2 event2
                         3 event3}
              events [1 2 3]
              open-events #{1}
              threaded-events (thread-events events event-map open-events)]
          (should= [1 3 2] (map :id threaded-events))
          (should= [0 1 0] (map :indent threaded-events))))

  (it "treats all articles threaded below an open article as open"
          (let [event1 {:id 1 :references [2]}
                event2 {:id 2 :references [3]}
                event3 {:id 3}
                event-map {1 event1
                           2 event2
                           3 event3}
                events [1 2 3]
                open-events #{1}
                threaded-events (thread-events events event-map open-events)]
            (should= [1 2 3] (map :id threaded-events))
            (should= [0 1 2] (map :indent threaded-events))))
  )
