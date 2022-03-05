(ns more-speech.ui.header-window-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.text-window :refer [text-window-controls
                                                setup-text-frame]]
            [more-speech.ui.header-window :refer :all]
            [more-speech.ui.widget :refer [widget setup-widget]]
            [more-speech.ui.graphics :as g]
            [more-speech.ui.config :as config]
            [more-speech.ui.cursor :as cursor]
            [clojure.spec.alpha :as s]))

(defrecord mock-graphics []
  g/graphics
  (line-height [_graphics]
    20)
  )

(defrecord mock-controls []
  text-window-controls
  (get-element-height [_c _state]
    2))

(defrecord mock-widget []
  widget)

(declare state frame
         text-event nicknames
         events)

(describe "article frame"
  (with state {:application
               {:graphics (->mock-graphics)}})
  (with frame {:x 0 :y 0 :w 500 :h 500 :controls (->mock-controls)})
  (context "setup"
    (it "determines number of article headers fit in the frame"
      (let [frame (setup-text-frame @state @frame)]
        (should= 250 (:n-elements frame)))
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
    (it "adds thread buttons to the frame"
      (let [frame (add-thread-buttons {:path [:frame]} [{:id 111} {:id 222}])]
        (should= 111 (get-in frame ["T1" :id]))
        (should= 222 (get-in frame ["T2" :id]))
        (should= [:frame "T1"] (get-in frame ["T1" :path]))
        (should= [:frame "T2"] (get-in frame ["T2" :path]))))

    (it "creates buttons for header selection"
      (let [headers [{:id 1}
                     {:id 2}]
            button-creator (make-button-creator @state @frame)
            buttons (create-selection-buttons button-creator headers)
            header-height (+ config/header-top-margin
                             config/header-bottom-margin
                             (* config/header-lines 20))]
        (should= [1 2] (map :id buttons))
        (should= [true true] (map #(satisfies? widget %) buttons))
        (should=
          [config/header-top-margin
           (+ config/header-top-margin header-height)]
          (map :y buttons))
        (should= 20 (:x (first buttons)))
        (should= 40 (:h (first buttons)))
        (should= 480 (:w (first buttons)))))

    (it "adds selection buttons to the frame"
      (let [frame (add-selection-buttons {:path [:frame]} [{:id 111} {:id 222}])]
        (should= 111 (get-in frame ["S1" :id]))
        (should= 222 (get-in frame ["S2" :id]))
        (should= [:frame "S1"] (get-in frame ["S1" :path]))
        (should= [:frame "S2"] (get-in frame ["S2" :path]))))
    )

  (context "Formatting Headers"
    (it "abbreviates authors"
      (should= "short" (abbreviate-author "short"))
      (should= "a very long autho..."
               (abbreviate-author "a very long author name")))


    (it "abbreviates bodies"
      (should= "short" (abbreviate-body "short"))
      (let [abbreviated (abbreviate-body (apply str (repeat 200 "*")))]
        (should= "***" (.substring abbreviated 0 3))
        (should= "..." (.substring abbreviated (- (count abbreviated) 3)))))


    (let [article {:group "comp.lang.c++"
                   :author "Bob"
                   :time 1642683327
                   :subject "Subject"
                   :body "My Message to you."
                   :thread-count 15}]
      (it "conforms to spec."
        (should (s/valid? ::article article)))

      (it "is properly formatted"
        (let [markup (markup-header article)]
          (should (vector? markup))
          (should (every? cursor/valid-markup-token? markup))))))

  (context "threading of events"
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

  (context "Moving the header selection."
    (with events [{:id 1} {:id 2} {:id 3}])
    (it "finds the index of a header id."
        (should-be-nil (index-of-selection @events 4))
        (should= 0 (index-of-selection @events 1))
        (should= 1 (index-of-selection @events 2))
        (should= 2 (index-of-selection @events 3)))

    (it "gets id of a moved selection."
      (should= 1 (id-of-selection-moved-by @events 2 -1))
      (should= 1 (id-of-selection-moved-by @events 3 -2))
      (should= 3 (id-of-selection-moved-by @events 1 2))
      (should= nil (id-of-selection-moved-by @events 1 4))
      )
    )
  )
