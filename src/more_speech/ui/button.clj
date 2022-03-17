(ns more-speech.ui.button
  (:require [more-speech.ui.widget :as w]
            [more-speech.ui.graphics :as g]
            [more-speech.util.geometry :as util]
            [more-speech.ui.config :as config]
            [more-speech.ui.app-util :as app-util]))

(declare update-button)

(defrecord button [x y h w button-state left-up]
  w/widget
  (setup-widget [widget _state]
    widget)
  (update-widget [widget state]
    (update-button widget state))
  (draw-widget [widget state]
    (let [draw (:draw widget)
          g (app-util/get-graphics state)]
      (draw g widget)))
  )

(defn- get-button-state [which]
  (if (nil? which)
    :in
    which))

(defn- check-call-left-held [state button which]
  (let [button (get-in state (:path button))
        left-held (:left-held button)]
    (if (and left-held
             (= :left which))
      (left-held button state)
      state)))

(defn update-button [button state]
  (let [g (app-util/get-graphics state)
        [mx my which] (g/get-mouse g)
        button (get-in state (:path button))
        {:keys [x y w h]} button
        in? (util/inside-rect [x y w h] [mx my])
        lock (w/get-mouse-lock state)
        button-target? (if (nil? lock)
                         in?
                         (= lock (:path button)))
        button-state (if button-target?
                       (get-button-state which)
                       :out)
        state (if button-target?
                (check-call-left-held state button which)
                state)
        ]
    (assoc-in state (concat (:path button) [:button-state]) button-state))
  )

(defn up-arrow [graphics {:keys [x y w h button-state]}]
  (g/stroke graphics config/black)
  (let [weight (if (= button-state :in) 2 1)
        fill (if (= button-state :left) config/black config/no-fill)
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
        fill (if (= button-state :left) config/black config/no-fill)
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

(defn draw-thumb [graphics {:keys [x y w h button-state]}]
  (g/stroke graphics config/black)
  (let [weight (if (= button-state :in) 2 1)
        fill (if (= button-state :left)
               config/thumb-dragged
               config/thumb-normal)]
    (g/with-translation
      graphics [x y]
      (fn [graphics]
        (g/stroke-weight graphics weight)
        (g/fill graphics fill)
        (g/rect graphics [0 0 w h])))))
