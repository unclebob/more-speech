(ns more-speech.ui.button-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.button :refer :all]
            [more-speech.ui.widget :as w]
            [more-speech.ui.graphics :as g]))

(defrecord mouse-pos [x y which]
  g/graphics
  (get-mouse [graphics] [x y which]))

(defn- left-up [button state]
  (assoc-in state (conj (:path button) :left-came-up) true))

(declare b)
(describe "mouse position within button"
  (with b (map->button {:x 10 :y 10 :w 10 :h 10
                        :button-state :whatever
                        :left-up left-up
                        :path [:application :button]}))
  (it "is :out if mouse is not in the rectangle."
    (let [mock-g (->mouse-pos 0 0 nil)
          state (w/update-widget @b {:application {:graphics mock-g}})
          b (get-in state [:application :button])]
      (should= :out (:button-state b))))

  (it "is :in if mouse is in rectangle."
    (let [mock-g (->mouse-pos 10 10 nil)
          state (w/update-widget @b {:application {:graphics mock-g}})
          b (get-in state [:application :button])]
      (should= :in (:button-state b))))

  (it "is :left if mouse is in rectangle and left is down."
      (let [mock-g (->mouse-pos 10 10 :left)
            state (w/update-widget @b {:application {:graphics mock-g}})
            b (get-in state [:application :button])]
        (should= :left (:button-state b))))

  (it "is :right if mouse is in rectangle and right is down."
      (let [mock-g (->mouse-pos 10 10 :right)
            state (w/update-widget @b {:application {:graphics mock-g}})
            b (get-in state [:application :button])]
        (should= :right (:button-state b))))

  (it "calls :left-up if left button comes up while inside."
        (let [mock-g (->mouse-pos 10 10 nil)
              b (assoc @b :button-state :left)
              state (w/update-widget b {:application {:graphics mock-g
                                                      :button b}})
              b (get-in state [:application :button])]
          (should (:left-came-up b))))
  )