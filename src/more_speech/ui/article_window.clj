(ns more-speech.ui.article-window
  (:require
    [more-speech.ui.text :as text]
    [more-speech.article :as a]
    [more-speech.ui.widget :refer [widget]]
    [more-speech.ui.graphics :as g]))

(declare draw-article-window)

(defrecord article-window [x y w h fonts]
  widget
  (setup-widget [widget state])
  (update-widget [widget state])
  (draw-widget [widget state]
    (draw-article-window (:application state) widget))
  (mouse-up [widget state position])
  (mouse-down [widget state position])
  )

(defn draw-article [g window cursor article]
  (g/text-align g [:left])
  (g/fill g [0 0 0])
  (text/render cursor window (a/markup-article article))
  )

(defn draw-articles [g application {:keys [fonts] :as window}]
  (loop [cursor (text/->cursor 0 (text/line-height) 5)
         articles (take 20 (:articles application))]
    (if (empty? articles)
      cursor
      (recur (draw-article g window cursor (first articles))
             (rest articles)))))

(defn draw-article-window [application window]
  (let [g (:graphics application)]
    (g/with-translation
      g [(:x window) (:y window)]
      (fn [g]
        (g/stroke g [0 0 0])
        (g/stroke-weight g 2)
        (g/fill g [255 255 255])
        (g/rect g [0 0 (:w window) (:h window)])
        (draw-articles g application window))
      )))
