(ns more-speech.ui.widget-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.widget :as w :refer :all]
            [clojure.spec.alpha :as s]))

(defrecord child []
  widget
  (setup-widget [widget state]
    (let [path (:path widget)
          widget (assoc widget :setup true)
          state (assoc-in state path widget)]
      state))
  )

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
      (let [
            child-1 (assoc (->child) :path [:parent :child-1])
            child-2 (assoc (->child) :path [:parent :child-2])
            parent {:path [:parent] :child-1 child-1 :child-2 child-2}
            state {:parent parent}
            f (fn [widget state]
                (let [widget (assoc widget :did-f true)
                      state (assoc-in state (:path widget) widget)]
                  state))
            state (do-children parent state f)]
        (should (get-in state [:parent :child-1 :did-f] false))
        (should (get-in state [:parent :child-2 :did-f] false)))))

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
        (should (get-in state [:parent :child :grand-child :setup])))))
  )