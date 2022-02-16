(ns more-speech.ui.widget-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.application :refer [map->application]]
            [more-speech.ui.graphics :as g]
            [more-speech.ui.widget :refer :all]))

(defrecord child []
  widget
  (setup-widget [widget state]
    (assoc widget :setup true)))

(defrecord child-with-child []
  widget
  (setup-widget [widget state]
    (assoc widget :child (->child))))

(defn- f [widget state] (assoc-in state (conj (:path widget) :did-f) true))

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

(defrecord mock-widget [x y w h]
  widget
  (setup-widget [widget state]
    widget))

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
            widget2-1 (->mock-widget  35 15 5 5)
            widget2 (assoc widget2 :w2-1 widget2-1);
            application (map->application {:path [:application] :w1 widget1 :w2 widget2})
            state {:application application}
            state (setup-child-widgets application state)
            application (:application state)
            target (find-deepest-mouse-target application 36 16)]
        (should= [:application :w2 :w2-1] (:path target)))
      )
    )
  )