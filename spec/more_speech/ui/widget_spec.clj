(ns more-speech.ui.widget-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.application :refer [map->application]]
            [more-speech.ui.widget :refer :all]
            [more-speech.ui.graphics :as g]))

(defrecord child []
  widget
  (setup-widget [widget _state]
    (assoc widget :setup true)))

(defrecord child-with-child []
  widget
  (setup-widget [widget _state]
    (assoc widget :child (->child))))

(defn- f [widget state] (assoc-in state (concat (:path widget) [:did-f]) true))

(describe "Widgets"
  (context "get child widgets"
    (it "gets one child"
      (let [parent {:child (->child)
                    :not-child "not-child"}]
        (should= [:child] (get-child-widgets parent))))

    (it "gets no children"
      (let [parent {:not-child "not-child"}]
        (should= [] (get-child-widgets parent)))
      )

    (it "gets many children"
      (let [parent {:child-1 (->child)
                    :child-2 (->child)
                    :not-child "not-child"}]
        (should= #{:child-1 :child-2} (set (get-child-widgets parent))))))

  (context "do-children"
    (it "does child widgets"
      (let [child-1 (assoc (->child) :path [:parent :child-1])
            child-2 (assoc (->child) :path [:parent :child-2])
            parent {:path [:parent] :child-1 child-1 :child-2 child-2}
            state {:parent parent}
            state (update-children parent state f)]
        (should (get-in state [:parent :child-1 :did-f] false))
        (should (get-in state [:parent :child-2 :did-f] false))))

    (it "does the hierarchy."
      (let [grandchild (->child)
            child (assoc (->child) :child grandchild)
            parent {:path [:parent] :child child}
            state {:parent parent}
            state (setup-child-widgets parent state)
            state (update-children (:parent state) state f)]
        (should (get-in state [:parent :child :did-f] false))
        (should (get-in state [:parent :child :child :did-f] false)))))

  (context "can be setup"
    (it "constructs the path and calls setup for children."
      (let [parent {:path [:parent]
                    :child (->child)}
            state {:parent parent}
            state (setup-child-widgets parent state)]
        (should (get-in state [:parent :child :setup] false))
        (should= [:parent :child] (get-in state [:parent :child :path]))))

    (it "is harmless if the parent has no children."
      (let [parent {:path [:parent]}
            state {:parent parent}
            state (setup-child-widgets parent state)]
        (should= {:parent parent} state)))

    (it "handles multiple children."
      (let [parent {:path [:parent]
                    :child-1 (->child)
                    :child-2 (->child)}
            state {:parent parent}
            state (setup-child-widgets parent state)]
        (should (get-in state [:parent :child-1 :setup] false))
        (should (get-in state [:parent :child-2 :setup] false))
        (should= [:parent :child-1] (get-in state [:parent :child-1 :path]))
        (should= [:parent :child-2] (get-in state [:parent :child-2 :path]))))

    (it "handles a hierarchy"
      (let [grand-child (->child)
            child (assoc (->child) :grand-child grand-child)
            parent {:path [:parent] :child child}
            state {:parent parent}
            state (setup-child-widgets parent state)]
        (should (get-in state [:parent :child :setup]))
        (should (get-in state [:parent :child :grand-child :setup]))))

    (it "handles a hierarchy setup by children."
      (let [child (->child-with-child)
            parent {:path [:parent] :child child}
            state {:parent parent}
            state (setup-child-widgets parent state)]
        (should (get-in state [:parent :child :child :setup])))
      )
    )
  )

(declare application
         state
         child-widget
         grandchild-widget)

(defrecord mock-widget [x y w h]
  widget
  (setup-widget [widget _state]
    widget))


(describe "update widgets"
  (with grandchild-widget (map->mock-widget {:path [:application :child :grandchild]}))
  (with child-widget (map->mock-widget {:path [:application :child]
                                        :grandchild @grandchild-widget}))

  (with state {:application {:path [:application]
                             :next-update #{}
                             :this-update #{}
                             :child @child-widget}})

  (it "adds a widget path"
    (let [widget-path (:path @child-widget)
          state (redraw-widget @state widget-path)
          update-widgets (get-in state [:application :next-update])]
      (should (contains? update-widgets widget-path))))

  (it "adds a widget"
    (let [widget-path (:path @child-widget)
          state (redraw-widget @state @child-widget)
          update-widgets (get-in state [:application :next-update])]
      (should (contains? update-widgets widget-path))))

  (it "detects widget not updated"
    (should-not (redraw-widget? @state @child-widget)))

  (it "detects an updated widget"
    (let [state (redraw-widget @state @child-widget)
          state (assoc-in state
                          [:application :this-update]
                          (get-in state [:application :next-update]))]
      (should (redraw-widget? state @child-widget))))

  (it "updates children"
    (let [state (redraw-widget @state @child-widget)
          state (assoc-in state
                          [:application :this-update]
                          (get-in state [:application :next-update]))]
      (should (redraw-widget? state @grandchild-widget))))

  (it "does not update parents"
    (let [state (redraw-widget @state @grandchild-widget)]
      (should-not (redraw-widget? state @child-widget))))

  (it "checks for ancestors"
    (should (is-ancestor? [:x] [:x]))
    (should (is-ancestor? [:x] [:x :y]))
    (should (is-ancestor? [:x :y] [:x :y]))
    (should (is-ancestor? [:x :y] [:x :y :z]))
    (should (is-ancestor? [:x :y] [:x :y :z :a :b]))

    (should-not (is-ancestor? [] [:x]))
    (should-not (is-ancestor? [:x] [:y]))
    (should-not (is-ancestor? [:x :y] [:x :z]))
    (should-not (is-ancestor? [:x :y :z] [:x :y]))))

(describe "clear-widgets"
  (it "clears out the widgets from the header frame"
    (let [frame {:x :not-widget :w1 (->mock-widget 0 0 0 0)}
          frame (clear-widgets frame)]
      (should-be-nil (:w1 frame))
      (should= {:x :not-widget} frame))))

(defrecord mock-graphics [x y]
  g/graphics
  (get-mouse [_graphics]
    [x y :left]))

(describe "mouse operations"
  (context "find mouse target"
    (it "does not return a target if there are no application widgets."
      (let [application (map->application {:path [:application]})
            target (find-deepest-mouse-target application 0 0)]
        (should-be-nil target)))

    (it "does not return a target if the mouse is not in the top widgets."
      (let [widget1 (->mock-widget 10 10 10 10)
            widget2 (->mock-widget 30 10 10 10)
            application (map->application {:path [:application] :w1 widget1 :w2 widget2})
            state {:application application}
            state (setup-child-widgets application state)
            application (:application state)
            target (find-deepest-mouse-target application 0 0)]
        (should-be-nil target)))

    (it "finds first top widget with mouse inside when there are no children."
      (let [widget1 (->mock-widget 10 10 10 10)
            widget2 (->mock-widget 30 10 10 10)
            application (map->application {:path [:application] :w1 widget1 :w2 widget2})
            state {:application application}
            state (setup-child-widgets application state)
            application (:application state)
            target (find-deepest-mouse-target application 11 11)]
        (should= [:application :w1] (:path target))))

    (it "finds deepest child containing the mouse."
      (let [widget1 (->mock-widget 10 10 10 10)
            widget2 (->mock-widget 30 10 20 20)
            widget2-1 (->mock-widget 35 15 5 5)
            widget2 (assoc widget2 :w2-1 widget2-1)         ;
            application (map->application {:path [:application] :w1 widget1 :w2 widget2})
            state {:application application}
            state (setup-child-widgets application state)
            application (:application state)
            target (find-deepest-mouse-target application 36 16)]
        (should= [:application :w2 :w2-1] (:path target)))
      )

    (it "finds deepest responder"
      (let [widget1 (->mock-widget 10 10 10 10)
            widget2 (->mock-widget 30 10 20 20)
            widget2-1 (->mock-widget 35 15 5 5)
            widget2 (assoc widget2 :w2-1 widget2-1
                                   :responder (fn [] :place-holder))
            application (map->application {:path [:application]
                                           :graphics (->mock-graphics 36 16)
                                           :w1 widget1 :w2 widget2})
            state {:application application}
            state (setup-child-widgets application state)
            target (find-mouse-responder state :responder)]
        (should= [:application :w2] (:path target)))
      )

    (it "finds locked responder"
          (let [widget1 (->mock-widget 10 10 10 10)
                widget2 (->mock-widget 30 10 20 20)
                widget2-1 (->mock-widget 35 15 5 5)
                widget1 (assoc widget1 :responder (fn [] :place-holder))
                widget2 (assoc widget2 :w2-1 widget2-1
                                       :responder :place-holder)
                application (map->application {:path [:application]
                                               :graphics (->mock-graphics 36 16)
                                               :mouse-locked-to [:application :w1]
                                               :w1 widget1 :w2 widget2})
                state {:application application}
                state (setup-child-widgets application state)
                target (find-mouse-responder state :responder)]
            (should= [:application :w1] (:path target)))
          )
    )
  )