(ns more-speech.ui.author-window
  (:require
    [clojure.string :as string]
    [more-speech.ui.widget :refer [widget]]
    [more-speech.ui.text :as text]
    [more-speech.article :as a]
    [more-speech.ui.graphics :as g]))

(declare draw-author-window)

(defrecord author-window [x y w h fonts]
  widget
  (setup-widget [widget state])
  (update-widget [widget state])
  (draw-widget [widget state]
    (draw-author-window (:application state) widget))
  (mouse-up [widget state position])
  (mouse-down [widget state position])
  )

(defn draw-author [g window cursor author]
  (g/text-align g [:left])
  (g/text-color g [0 0 0])
  (text/render cursor window (a/markup-author author)))

(defn draw-authors [g application window]
  (g/text-align g [:left])
  (g/text-color g [0 0 0])
  (loop [cursor (text/->cursor 0 (text/line-height) 5)
         authors (take-last 60 (sort-by #(string/lower-case (text/nil->blank (second %))) (:nicknames application)))]
    (if (empty? authors)
      cursor
      (recur (draw-author g window cursor (first authors))
             (rest authors))))
  )

(defn draw-author-window [application window]
  (let [g (:graphics application)]
    (g/with-translation
      g [(:x window) (:y window)]
      (fn [g] (g/stroke g [0 0 0])
        (g/stroke-weight g 2)
        (g/fill g [255 255 255])
        (g/rect g [0 0 (:w window) (:h window)])
        (draw-authors g application window)))))