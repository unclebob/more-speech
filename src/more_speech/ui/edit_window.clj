(ns more-speech.ui.edit-window
  (:require [more-speech.ui.text-window :refer [text-window-controls]]
            [more-speech.ui.cursor :as cursor]
            [more-speech.ui.graphics :as g]
            [more-speech.ui.config :as config]
            [clojure.spec.alpha :as s]
            [more-speech.nostr.events :as events]
            [clojure.string :as string]
            [clojure.core.async :as async]))

(s/def ::text (and vector? (s/coll-of string?)))
(s/def ::insertion-point (s/tuple [int? int?]))
(s/def ::edit-frame (s/keys :opt-un [::text
                                     ::insertion-point]))

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
        [x y] (get frame :insertion-point [0 0])]
    (loop [cursor (cursor/->cursor g 0 (g/line-height g) 20)
           lines (get frame :text [""])
           line-num 0]
      (if (empty? lines)
        nil
        (let [line (first lines)
              insertion-cursor (cursor/set-pos cursor x)
              cursor (cursor/draw-text cursor line)
              cursor (cursor/new-lines cursor 1)
              cursor (cursor/set-x cursor 0)]
          (when (= line-num y)
            (cursor/draw insertion-cursor))
          (recur cursor (rest lines) (inc line-num))))))
  state
  )

(declare add-char delete-char move-insertion send-msg)

(defn edit-window-key-pressed [state frame {:keys [key raw-key]}]
  (condp = key
    :shift state
    :alt state
    :command state
    :unknown-key state
    :control state

    :right (move-insertion state frame [1 0])
    :left (move-insertion state frame [-1 0])
    :up (move-insertion state frame [0 -1])
    :down (move-insertion state frame [0 1])

    (condp = (int raw-key)
      config/ctrl-s (send-msg state frame)

      (let [frame (if (= \backspace raw-key)
                    (delete-char frame)
                    (add-char frame raw-key))
            frame-path (:path frame)]
        (assoc-in state frame-path frame))))
  )

(defn move-insertion [state frame [x y]]
  (let [[ix iy] (get frame :insertion-point [0 0])
        text (get frame :text [""])
        line (nth text iy)
        max-x (count line)
        max-y (dec (count text))
        ix (+ ix x)
        iy (+ iy y)
        ix (max 0 ix)
        ix (min max-x ix)
        iy (max 0 iy)
        iy (min max-y iy)
        frame (assoc frame :insertion-point [ix iy])
        frame-path (:path frame)
        ]
    (assoc-in state frame-path frame)))

(defn add-char [frame char]
  (let [text (vec (get frame :text [""]))
        [x y] (get frame :insertion-point [0 0])
        line (nth text y)]
    (cond
      (= char \newline)
      (let [line1 (subs line 0 x)
            line2 (subs line x)
            text1 (take y text)
            text2 (drop (inc y) text)
            text (concat text1 [line1 line2] text2)
            frame (assoc frame :text (vec text)
                               :insertion-point [0 (inc y)])]
        frame)

      :else
      (let [line (str (subs line 0 x) char (subs line x))
            text (assoc text y line)
            frame (assoc frame :text (vec text)
                               :insertion-point [(inc x) y])]
        frame))))

(defn delete-char [frame]
  (let [text (vec (get frame :text [""]))
        [x y] (get frame :insertion-point [0 0])
        line (nth text y)]
    (if (zero? x)
      (if (zero? y)
        frame
        (let [prev-line (nth text (dec y))
              new-line (str prev-line line)
              text (assoc text (dec y) new-line)
              text (assoc text y nil)
              text (remove nil? text)
              frame (assoc frame :text text
                                 :insertion-point [(count prev-line) (dec y)])]
          frame))
      (let [line-head (subs line 0 (dec x))
            line-tail (subs line x)
            text (assoc text y (str line-head line-tail))
            frame (assoc frame :text text
                               :insertion-point [(dec x) y])]
        frame))
    )
  )

(defn send-msg [state frame]
  (prn 'send-message)
  (let [private-key (get-in state [:keys :private-key])
        text (string/join \newline (:text frame))
        event (events/compose-text-event private-key text)
        send-chan (:send-chan state)
        frame (assoc frame :text [""] :insertion-point [0 0])
        state (assoc-in state (:path frame) frame)]
    (async/>!! send-chan [:event event])
    state))
