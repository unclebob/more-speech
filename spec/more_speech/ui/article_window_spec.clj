(ns more-speech.ui.article-window-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.article-window :refer :all]
            [more-speech.ui.widget :refer [widget setup-widget]]
            [more-speech.ui.graphics :as g]
            [more-speech.ui.config :as config]
            [more-speech.ui.button :as b]))

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

(defrecord mock-graphics []
  g/graphics
  (line-height [graphics]
    20)
  )

(defrecord mock-widget []
  widget)

(declare state frame
         text-event nicknames)

(describe "article frame"
  (with state {:application
               {:graphics (->mock-graphics)}})
  (with frame {:x 0 :y 0 :w 500 :h 500})
  (context "setup"
    (it "determines number of article headers fit in the frame"
      (let [frame (setup-header-frame @state @frame)]
        (should= 10 (:n-headers frame)))
      )
    )

  (context "update"
    (with text-event {:pubkey 42
                      :created-at 1000
                      :content "the content"
                      :references [1]
                      :indent 1})
    (with nicknames {42 "name"})

    (it "converts a text event to a header."
      (should= {:id nil,
                :group "",
                :author "name",
                :subject "?",
                :time 1000,
                :body "the content",
                :thread-count 1,
                :indent 1}
               (event->header @text-event @nicknames)))

    (it "clears out the widgets from the header frame"
      (let [frame {:x :not-widget :w1 (->mock-widget)}
            frame (clear-widgets frame)]
        (should-be-nil (:w1 frame))
        (should= {:x :not-widget} frame)))

    (it "creates buttons for threads"
      (let [headers [{:id 1 :thread-count 1 :indent 0}
                     {:id 2 :thread-count 1 :indent 1}
                     {:id 3 :thread-count 1 :indent 0}
                     {:id 4 :thread-count 0 :indent 0}]
            open-thread #{1}
            state (assoc-in @state [:application :open-thread] open-thread)
            button-creator (make-button-creator state @frame)
            buttons (create-thread-buttons button-creator headers)
            header-height (+ config/header-top-margin
                             config/header-bottom-margin
                             (* config/header-lines 20))]
        (should= [1 3] (map :id buttons))
        (should= [draw-minus draw-plus]
                 (map :draw buttons))
        (should= [true true] (map #(satisfies? widget %) buttons))
        (should=
          [config/header-top-margin
           (+ config/header-top-margin (* 2 header-height))]
          (map :y buttons)))
      )
    (it "adds buttons to the frame"
      (let [frame (add-thread-buttons {} [{:id 1} {:id 2}])]
        (should= #{1 2} (set (keys frame)))))
    )
  )
