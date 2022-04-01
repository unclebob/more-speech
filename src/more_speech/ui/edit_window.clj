(ns more-speech.ui.edit-window
  (:require [more-speech.ui.text-window :refer [text-window-controls]]
            [more-speech.ui.cursor :as cursor]
            [more-speech.ui.graphics :as g]
            [more-speech.ui.config :as config]))


(declare draw-edit-frame)

(defrecord edit-window-controls []
  text-window-controls
  (get-element-height [_controls _state]
    1)
  (draw-elements [_controls state frame]
    (draw-edit-frame state frame))
  (update-elements [_controls state _frame]
    state)
  (key-pressed [_controls state _frame _key]
    state)
  )

(defn draw-edit-frame [state _frame]
  (let [application (:application state)
        g (:graphics application)
        cursor (cursor/->cursor g 0 (g/line-height g) 20)]
    (g/text-align g [:left])
    (g/text-color g config/black)
    (cursor/draw cursor))
  state
  )
