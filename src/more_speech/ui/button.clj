(ns more-speech.ui.button
  (:require [more-speech.ui.widget :as w]
            [quil.core :as q]))

(defrecord button [x y h w]
  w/widget
  (setup-widget [widget state]
    widget)
  (draw-widget [widget state]
    (q/stroke-weight 2)
    (q/ellipse-mode :corner)
    (q/ellipse 0 0 (- w 2) (- h 2))
    widget)

  )
