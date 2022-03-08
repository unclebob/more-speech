(ns more-speech.ui.edit-window
  (:require [more-speech.ui.text-window :refer [text-window-controls]]))

(defrecord edit-window-controls []
  text-window-controls
  (get-element-height [_controls _state]
    1)
  (draw-elements [_controls state _frame]
    state)
  (update-elements [_controls state _frame]
    state)
  (key-pressed [_controls state _frame _key]
    state)
  )
