(ns more-speech.ui.author-window
  (:require
    [clojure.string :as string]
    [more-speech.ui.widget :refer [widget]]
    [more-speech.ui.cursor :as text]
    [more-speech.article :as a]
    [more-speech.ui.graphics :as g]))

(declare draw-author-window)

(defrecord author-window [x y w h fonts]
  widget
  (setup-widget [widget state]
    widget)
  (update-widget [widget state]
    [widget state])
  (draw-widget [widget state]
    (draw-author-window (:application state) widget))
  )

(defn draw-author [window cursor author]
  (let [g (:graphics cursor)]
    (g/text-align g [:left])
    (g/text-color g [0 0 0])
    (text/render cursor window (a/markup-author author))))

(defn draw-authors [application window]
  (let [g (:graphics application)]
    (g/text-align g [:left])
    (g/text-color g [0 0 0])
    (loop [cursor (text/->cursor g 0 (g/line-height g) 5)
           authors (take-last 60 (sort-by #(string/lower-case (text/nil->blank (second %))) (:nicknames application)))]
      (if (empty? authors)
        cursor
        (recur (draw-author window cursor (first authors))
               (rest authors)))))
  )

(defn draw-author-window [application window]
  (let [g (:graphics application)]
    (g/with-translation
      g [(:x window) (:y window)]
      (fn [g] (g/stroke g [0 0 0])
        (g/stroke-weight g 2)
        (g/fill g [255 255 255])
        (g/rect g [0 0 (:w window) (:h window)])
        (draw-authors application window)))))