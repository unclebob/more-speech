(ns more-speech.ui.edit-window
  (:require [more-speech.ui.text-window :refer [text-window-controls]]
            [more-speech.ui.cursor :as cursor]
            [more-speech.ui.graphics :as g]
            [more-speech.ui.config :as config]))


(declare draw-edit-frame
         edit-window-key-pressed)

(defrecord edit-window-controls []
  text-window-controls
  (get-element-height [_controls _state]
    1)
  (draw-elements [_controls state frame]
    (draw-edit-frame state frame))
  (update-elements [_controls state _frame]
    state)
  (key-pressed [_controls state frame key]
    (edit-window-key-pressed state frame key))
  )

(defn draw-edit-frame [state frame]
  (let [application (:application state)
        g (:graphics application)
        _ (g/text-align g [:left])
        _ (g/text-color g config/black)
        cursor (cursor/->cursor g 0 (g/line-height g) 20)
        text (get frame :text "")
        cursor (cursor/draw-text cursor text)]
    (cursor/draw cursor))
  state
  )

(declare add-char)
(defn edit-window-key-pressed [state frame key]
  (let [char (:raw-key key)
        frame (add-char frame char)
        frame-path (:path frame)]
    (assoc-in state frame-path frame))
  )

(defn add-char [frame char]
  (let [text (get frame :text "")
        text (str text char)
        frame (assoc frame :text text)]
    frame))
