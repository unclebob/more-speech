(ns more-speech.ui.button-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.button :refer :all]
            [more-speech.ui.widget :as w]
            [more-speech.ui.graphics :as g]))

(defrecord mouse-pos [x y which]
  g/graphics
  (get-mouse [graphics] [x y which]))

(defn- left-up [button state]
  [(assoc button :left-came-up true)
  state])

(describe "mouse position within button"
  (with b (map->button {:x 10 :y 10 :w 10 :h 10
                        :button-state :whatever
                        :left-up left-up}))
  (it "is :out if mouse is not in the rectangle."
    (let [mock-g (->mouse-pos 0 0 nil)
          [b _] (w/update-widget @b {:application {:graphics mock-g}})]
      (should= :out (:button-state b))))

  (it "is :in if mouse is in rectangle."
    (let [mock-g (->mouse-pos 10 10 nil)
          [b _] (w/update-widget @b {:application {:graphics mock-g}})]
      (should= :in (:button-state b))))

  (it "is :left if mouse is in rectangle and left is down."
      (let [mock-g (->mouse-pos 10 10 :left)
            [b _] (w/update-widget @b {:application {:graphics mock-g}})]
        (should= :left (:button-state b))))

  (it "is :right if mouse is in rectangle and right is down."
      (let [mock-g (->mouse-pos 10 10 :right)
            [b _] (w/update-widget @b {:application {:graphics mock-g}})]
        (should= :right (:button-state b))))

  (it "calls :left-up if left button comes up while inside."
        (let [mock-g (->mouse-pos 10 10 nil)
              b (assoc @b :button-state :left)
              [b _] (w/update-widget b {:application {:graphics mock-g}})]
          (should (:left-came-up b))))
  )