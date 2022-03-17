(ns more-speech.ui.edit-window
  (:require [more-speech.ui.text-window :refer [text-window-controls]]
            ))


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
  state
  )
