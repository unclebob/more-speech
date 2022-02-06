(ns more-speech.ui.button-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.button :refer :all]
            [more-speech.ui.widget :as w]
            [more-speech.ui.graphics :as g]))

(defrecord mock-graphic [x y which time]
  g/graphics
  (get-mouse [graphics] [x y which])
  (get-time [graphics] time))

(defn- left-up [button state]
  (assoc-in state (conj (:path button) :left-came-up) true))

(defn- left-down [button state]
  (assoc-in state (conj (:path button) :left-went-down) true))

(defn- left-held [button state]
  (assoc-in state (conj (:path button) :left-held-down) true))

(def NOW 1234)                                              ; mock time of day
(def mouse-out (map->mock-graphic {:x 0 :y 0}))
(def mouse-in (map->mock-graphic {:x 10 :y 10}))
(def mouse-in-left-down (map->mock-graphic {:x 10 :y 10
                                            :which :left
                                            :time NOW}))
(def mouse-in-right-down (map->mock-graphic {:x 10 :y 10 :which :right}))

(def b (map->button {:x 10 :y 10 :w 10 :h 10
                     :button-state :whatever
                     :left-up left-up
                     :left-down left-down
                     :left-held left-held
                     :path [:application :button]}))

(defn- make-state [graphics]
  {:application {:graphics graphics
                 :button b}})

(describe "mouse position and button state"
  (it "is :out if mouse is not in the rectangle."
    (let [state (make-state mouse-out)
          state (w/update-widget b state)
          b (get-in state [:application :button])]
      (should= :out (:button-state b))))

  (it "is :in if mouse is in rectangle."
    (let [state (make-state mouse-in)
          state (w/update-widget b state)
          b (get-in state [:application :button])]
      (should= :in (:button-state b))))

  (it "is :left if mouse is in rectangle and left is down."
    (let [state (make-state mouse-in-left-down)
          state (w/update-widget b state)
          b (get-in state [:application :button])]
      (should= :left (:button-state b))))

  (it "is :right if mouse is in rectangle and right is down."
    (let [state (make-state mouse-in-right-down)
          state (w/update-widget b state)
          b (get-in state [:application :button])]
      (should= :right (:button-state b))))

  (it "calls :left-up if left button comes up while inside."
    (let [state (make-state mouse-in)
          state (assoc-in state [:application :button :button-state] :left)
          state (w/update-widget b state)
          b (get-in state [:application :button])]
      (should (:left-came-up b))))

  (it "records when the left button goes down."
    (let [state (make-state mouse-in-left-down)
          state (assoc-in state [:application :button :button-state] :in)
          state (w/update-widget b state)
          b (get-in state [:application :button])]
      (should= NOW (:left-time b)))
    )

  (it "erases left button down time when left button comes up"
    (let [state (make-state mouse-in)
          state (assoc-in state [:application :button :button-state] :left)
          state (assoc-in state [:application :button :left-time] NOW)
          state (w/update-widget b state)
          b (get-in state [:application :button])]
      (should= nil (:left-time b))))

  (it "calls :left-down when left button goes down while in"
    (let [state (make-state mouse-in-left-down)
          state (assoc-in state [:application :button :button-state] :in)
          state (w/update-widget b state)
          b (get-in state [:application :button])]
      (should (:left-went-down b))))

  (it "does not call :left-down if left already down."
    (let [state (make-state mouse-in-left-down)
          state (assoc-in state [:application :button :button-state] :left)
          state (w/update-widget b state)
          b (get-in state [:application :button])]
      (should-not (:left-went-down b))))

  (it "calls :left-held if left already down."
      (let [state (make-state mouse-in-left-down)
            state (assoc-in state [:application :button :button-state] :left)
            state (w/update-widget b state)
            b (get-in state [:application :button])]
        (should (:left-held-down b))))

  )