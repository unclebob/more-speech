(ns more-speech.ui.button
  (:require [more-speech.ui.widget :as w]
            [quil.core :as q]
            [more-speech.ui.graphics :as g]
            [more-speech.util.geometry :as util]))

(declare update-button)

(defrecord button [x y h w button-state]
  w/widget
  (setup-widget [widget state]
    widget)
  (update-widget [widget state]
    (update-button widget state))
  (draw-widget [widget state]
    (q/ellipse x y w h))
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
        button (if (and (nil? which) (= :left previous-state))
                 ((:left-up button) button)
                 button)

        ]
    (assoc button :button-state button-state)
    )
  )
