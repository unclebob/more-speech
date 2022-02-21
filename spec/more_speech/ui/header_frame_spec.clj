(ns more-speech.ui.header-frame-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.text-frame :refer :all]
            [more-speech.ui.header-frame-functions :refer :all]
            [more-speech.ui.widget :refer [widget setup-widget]]
            [more-speech.ui.graphics :as g]
            [more-speech.ui.config :as config]))

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
         text-event nicknames)

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
    (it "adds buttons to the frame"
      (let [frame (add-thread-buttons {} [{:id 1} {:id 2}])]
        (should= #{1 2} (set (keys frame)))))
    )
  )
