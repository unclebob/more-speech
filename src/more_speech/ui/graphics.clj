(ns more-speech.ui.graphics
  (:require
    [quil.core :as q]
    ))

(defprotocol graphics
  (screen-height [graphics])
  (screen-width [graphics])
  (text-align [graphics alignment])
  (text-color [graphics color])
  (stroke [graphics color])
  (stroke-weight [graphics weight])
  (fill [graphics color])
  (rect [graphics rect])
  (with-translation [graphics translation f])
  )

(defrecord quil-graphics []
  graphics
  (screen-height [graphics]
    (q/screen-height))
  (screen-width [graphics]
    (q/screen-width))
  (text-align [graphics alignment]
    (apply q/text-align alignment))
  (text-color [graphics color]
    (apply q/fill color))
  (stroke [graphics color]
    (apply q/stroke color))
  (stroke-weight [graphics weight]
    (q/stroke-weight weight))
  (fill [graphics color]
    (apply q/fill color))
  (rect [graphics rect]
    (apply q/rect rect))
  (with-translation [graphics translation f]
    (q/with-translation translation (f graphics)))
  )
