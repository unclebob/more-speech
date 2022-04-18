(ns more-speech.ui.object-mother
  (:require
    [more-speech.ui.graphics :refer [graphics]]
    [more-speech.ui.widget :as w]
    [more-speech.ui.application :as app])
  )

(defrecord graphics-dummy []
  graphics
  (screen_height [this]
    1)
  (screen_width [this]
    )
  (text_align [this alignment]
    )
  (text_color [this color]
    )
  (stroke [this color]
    )
  (no_stroke [this]
    )
  (stroke_weight [this weight]
    )
  (fill [this color]
    )
  (no_fill [this]
    )
  (rect_mode [this mode]
    )
  (rect [this rect]
    )
  (line [this line]
    )
  (polygon [this points]
    )
  (with_translation [this translation f]
    )
  (text_font [this font]
    )
  (line_height [this]
    1)
  (pos_width [this pos]
    1)
  (text_width [this s]
    )
  (text [this text-spec]
    )
  (get_mouse [this]
    )
  (cursor [this mode]
    )
  (get_time [this]
    ))



(defn make-test-state []
  (let [graphics (->graphics-dummy)
        application (app/map->application {:path [:application] :graphics graphics})
        application (w/setup-widget application {})
        state {:application application}
        state (w/setup-child-widgets application state)]
    state))