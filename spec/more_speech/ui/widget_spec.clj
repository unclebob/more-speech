(ns more-speech.ui.widget-spec
  (:require [speclj.core :refer :all]
              [more-speech.ui.widget :as w :refer :all]
              [clojure.spec.alpha :as s]))

(defrecord child []
  widget
  (setup-widget [widget state]
    (let [path (:path widget)
          widget (assoc widget :setup true)
          state (assoc-in state path widget)
          state (setup-child-widgets widget state)]
      state))
  )

(describe "Widgets"
  (context "can be setup"
    (it "constructs the path and calls setup for children."
      (let [parent {:path [:parent]
                    :widgets {:child (->child)}}
            state {:parent parent}
            state (setup-child-widgets parent state)]
        (should (get-in state [:parent :widgets :child :setup] false))
        (should= [:parent :widgets :child] (get-in state [:parent :widgets :child :path] ))))

    (it "is harmless if the parent has no children."
      (let [parent {:path [:parent]}
            state {:parent parent}
            state (setup-child-widgets parent state)]
        (should= {:parent parent} state)))

    (it "handles multiple children."
      (let [parent {:path [:parent]
                    :widgets {:child-1 (->child)
                              :child-2 (->child)}}
            state {:parent parent}
            state (setup-child-widgets parent state)]
        (should (get-in state [:parent :widgets :child-1 :setup] false))
        (should (get-in state [:parent :widgets :child-2 :setup] false))
        (should= [:parent :widgets :child-1] (get-in state [:parent :widgets :child-1 :path]))
        (should= [:parent :widgets :child-2] (get-in state [:parent :widgets :child-2 :path]))))

    (it "handles a hierarchy"
      (let [grand-child (->child)
            child (assoc-in (->child) [:widgets :grand-child] grand-child)
            parent {:path [:parent] :widgets {:child child}}
            state {:parent parent}
            state (setup-child-widgets parent state)]
        (should (get-in state [:parent :widgets :child :setup]))
        (should (get-in state [:parent :widgets :child :widgets :grand-child :setup])))
      )

    )
  )