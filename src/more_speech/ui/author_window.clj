(ns more-speech.ui.author-window
  (:require
    [quil.core :as q]
    [clojure.string :as string]
    [more-speech.ui.widget :refer [widget]]
    [more-speech.ui.text :as text]
    [more-speech.article :as a]))

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

(defn draw-author [window cursor author]
  (q/text-align :left)
  (q/fill 0 0 0)
  (text/render cursor window (a/markup-author author)))

(defn draw-authors [application window]
  (q/text-align :left)
  (q/fill 0 0 0)
  (loop [cursor (text/->cursor 0 (text/line-height) 5)
         authors (take-last 60 (sort-by #(string/lower-case (text/nil->blank (second %))) (:nicknames application)))]
    (if (empty? authors)
      cursor
      (recur (draw-author window cursor (first authors))
             (rest authors))))
  )

(defn draw-author-window [application window]
  (q/with-translation
    [(:x window) (:y window)]
    (q/stroke 0 0 0)
    (q/stroke-weight 2)
    (q/fill 255 255 255)
    (q/rect 0 0 (:w window) (:h window))
    (draw-authors application window)
    ))