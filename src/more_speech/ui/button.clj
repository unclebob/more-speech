(ns more-speech.ui.button
  (:require [more-speech.ui.widget :as w]
            [more-speech.ui.graphics :as g]
            [more-speech.util.geometry :as util]))

(declare update-button)

(defrecord button [x y h w button-state left-up]
  w/widget
  (setup-widget [widget state]
    widget)
  (update-widget [widget state]
    (update-button widget state))
  (draw-widget [widget state]
    (let [draw (:draw widget)
          g (get-in state [:application :graphics])]
      (draw g widget)))
  )

(defn- get-button-state [in? which]
  (cond
    (and in? (nil? which))
    :in
    (and in? (= :left which))
    :left
    (and in? (= :right which))
    :right
    :else
    :out))

(defn update-button [button state]
  (let [g (get-in state [:application :graphics])
        [mx my which] (g/get-mouse g)
        {:keys [x y w h]} button
        previous-state (:button-state button)
        in? (util/inside-rect [x y w h] [mx my])
        button-state (get-button-state in? which)
        [button state] (if (and (nil? which) (= :left previous-state))
                         ((:left-up button) button state)
                         [button state])]
    [(assoc button :button-state button-state)
     state]
    )
  )

(defn up-arrow [graphics {:keys [x y w h button-state]}]
  (g/stroke graphics [0 0 0])
  (let [weight (if (= button-state :in) 2 1)
        fill (if (= button-state :left) [0 0 0] [nil])
        w2 (/ w 2)
        h2 (/ h 2)
        w3 (/ w 3)
        w23 (* 2 w3)
        pa [w2 0]
        pb [w h2]
        pc [w23 h2]
        pd [w23 h]
        pe [w3 h]
        pf [w3 h2]
        pg [0 h2]]
    (g/with-translation
      graphics [x y]
      (fn [graphics]
        (g/stroke-weight graphics weight)
        (g/fill graphics fill)
        (g/polygon graphics [pa pb pc pd pe pf pg pa])))))



(defn down-arrow [graphics {:keys [x y w h button-state]}]
  (g/stroke graphics [0 0 0])
    (let [weight (if (= button-state :in) 2 1)
          fill (if (= button-state :left) [0 0 0] [nil])
          w2 (/ w 2)
          h2 (/ h 2)
          w3 (/ w 3)
          w23 (* 2 w3)
          pa [w2 h]
          pb [w h2]
          pc [w23 h2]
          pd [w23 0]
          pe [w3 0]
          pf [w3 h2]
          pg [0 h2]]
      (g/with-translation
        graphics [x y]
        (fn [graphics]
          (g/stroke-weight graphics weight)
          (g/fill graphics fill)
          (g/polygon graphics [pa pb pc pd pe pf pg pa])))))
