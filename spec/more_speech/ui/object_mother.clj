(ns more-speech.ui.object-mother
  (:require
    [more-speech.ui.graphics :as g]
    [more-speech.ui.widget :as w]
    [more-speech.ui.application :as app])
  )

(defrecord graphics-dummy []
  g/graphics
  (screen-height [_graphics]
    1)
  (screen-width [_graphics]
    )
  (text-align [_graphics _alignment]
    )
  (text-color [_graphics _color]
    )
  (stroke [_graphics _color]
    )
  (no-stroke [_graphics]
    )
  (stroke-weight [_graphics _weight]
    )
  (fill [_graphics _color]
    )
  (no-fill [_graphics]
    )
  (rect-mode [_graphics _mode]
    )
  (rect [_graphics _rect]
    )
  (line [_graphics _line]
    )
  (polygon [_graphics _points]
    )
  (with-translation [_graphics _translation _f]
    )
  (text-font [_graphics _font]
    )
  (line-height [_graphics]
    1)
  (pos-width [_graphics _pos]
    1)
  (text-width [_graphics _s]
    )
  (text [_graphics _text-spec]
    )
  (get-mouse [_graphics]
    )
  (cursor [_graphics _mode]
    )
  (get-time [_graphics]
    ))

(defn make-test-state []
  (let [graphics (->graphics-dummy)
        application (app/map->application {:path [:application] :graphics graphics})
        application (w/setup-widget application {})
        state {:application application}
        state (w/setup-child-widgets application state)]
    state))