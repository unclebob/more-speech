(ns more-speech.ui.button
  (:require [more-speech.ui.widget :as w]
            [quil.core :as q]))

(defrecord button [x y h w]
  w/widget
  (setup-widget [widget state]
    widget)
  (update-widget [widget state]
    widget)
  (draw-widget [widget state]
    (q/ellipse x y w h)
    widget)
  )
