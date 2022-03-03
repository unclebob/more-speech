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
  (no-stroke [graphics])
  (stroke-weight [graphics weight])
  (fill [graphics color])
  (no-fill [graphics])
  (rect-mode [graphics mode])
  (rect [graphics rect])
  (line [graphics line])
  (polygon [graphics points] "draws a polygon")
  (with-translation [graphics translation f])
  (text-font [graphics font])
  (line-height [graphics] "height of line in pixels.")
  (pos-width [graphics pos] "width of character position in pixels.")
  (text-width [graphics s] "Width in pixels of a string.")
  (text [graphics text-spec] "Draw text [s x y] or the like.")
  (get-mouse [graphics]
    "Returns [x y which].  x & y are absolute locations.
    which is :left | :right | :center | nil")
  (get-time [graphics]
    "returns current time in milliseconds.")
  )

(declare draw-polygon)

(defrecord quil-graphics [fonts]
  graphics
  (screen-height [_graphics]
    (q/height))
  (screen-width [_graphics]
    (q/width))
  (text-align [_graphics alignment]
    (apply q/text-align alignment))
  (text-color [_graphics color]
    (apply q/fill color))
  (stroke [_graphics color]
    (apply q/stroke color))
  (no-stroke [_graphics]
    (q/no-stroke))
  (stroke-weight [_graphics weight]
    (q/stroke-weight weight))
  (fill [_graphics color]
    (apply q/fill color))
  (no-fill [_graphics]
    (q/no-fill))
  (rect-mode [_graphics mode]
    (q/rect-mode mode))
  (rect [_graphics rect]
    (apply q/rect rect))
  (line [_graphics line]
    (apply q/line line))
  (polygon [_graphics points]
    (draw-polygon points))
  (with-translation [graphics translation f]
    (q/with-translation translation (f graphics)))
  (text-font [_graphics font]
    (q/text-font font))
  (line-height [_graphics]
    (+ (q/text-ascent) (q/text-descent)))
  (pos-width [_graphics pos]
      (let [s (apply str (repeat pos "X"))]
        (q/text-width s)))
  (text-width [_graphics s]
    (q/text-width s))
  (text [_graphics text-spec]
    (apply q/text text-spec))
  (get-mouse [_graphics]
    (let [x (q/mouse-x)
          y (q/mouse-y)
          which (if (q/mouse-pressed?) (q/mouse-button) nil)]
      [x y which]))
  (get-time [_graphics]
    (System/currentTimeMillis))
  )

(defn- draw-polygon [points]
  (q/begin-shape)
  (loop [points points]
    (if (empty? points)
      (q/end-shape)
      (do (apply q/vertex (first points))
          (recur (rest points))))))
