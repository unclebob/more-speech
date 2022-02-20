(ns more-speech.ui.app-util-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.app-util :refer :all]
            [more-speech.ui.widget :as w]))


(declare application
         state
         child-widget
         grandchild-widget)

(defrecord mock-widget []
  w/widget)

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
          state (update-widget @state widget-path)
          update-widgets (get-in state [:application :next-update])]
      (should (contains? update-widgets widget-path))))

  (it "adds a widget"
    (let [widget-path (:path @child-widget)
          state (update-widget @state @child-widget)
          update-widgets (get-in state [:application :next-update])]
      (should (contains? update-widgets widget-path))))

  (it "detects widget not updated"
    (should-not (update-widget? @state @child-widget)))

  (it "detects an updated widget"
    (let [state (update-widget @state @child-widget)
          state (assoc-in state
                          [:application :this-update]
                          (get-in state [:application :next-update]))]
      (should (update-widget? state @child-widget))))

  (it "updates children"
    (let [state (update-widget @state @child-widget)
          state (assoc-in state
                          [:application :this-update]
                          (get-in state [:application :next-update]))]
      (should (update-widget? state @grandchild-widget))))

  (it "does not update parents"
    (let [state (update-widget @state @grandchild-widget)]
      (should-not (update-widget? state @child-widget))))

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
    (let [frame {:x :not-widget :w1 (->mock-widget)}
          frame (clear-widgets frame)]
      (should-be-nil (:w1 frame))
      (should= {:x :not-widget} frame))))
