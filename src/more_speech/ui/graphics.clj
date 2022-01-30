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
  (line [graphics line])
  (with-translation [graphics translation f])
  (text-font [graphics font])
  (line-height [graphics] "height of line in pixels.")
  (pos-width [graphics pos] "width of character position in pixels.")
  (text-width [graphics s] "Width in pixels of a string.")
  (text [graphics text-spec] "Draw text [s x y] or the like.")
  (get-mouse [graphics]
    "Returns [x y which].  x & y are absolute locations.
    which is :left | :right | :center | nil")
  )

(defrecord quil-graphics [fonts]
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
  (line [graphics line]
    (apply q/line line))
  (with-translation [graphics translation f]
    (q/with-translation translation (f graphics)))
  (text-font [graphics font]
    (q/text-font font))
  (line-height [graphics]
    (+ (q/text-ascent) (q/text-descent)))
  (pos-width [graphics pos]
      (let [s (apply str (repeat pos "X"))]
        (q/text-width s)))
  (text-width [graphics s]
    (q/text-width s))
  (text [graphics text-spec]
    (apply q/text text-spec))
  (get-mouse [graphics]
    (let [x (q/mouse-x)
          y (q/mouse-y)
          which (if (q/mouse-pressed?) (q/mouse-button) nil)]
      [x y which]))
  )
