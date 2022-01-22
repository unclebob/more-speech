(ns more-speech.ui.article-window
  (:require
    [more-speech.ui.cursor :as text]
    [more-speech.article :as a]
    [more-speech.ui.widget :refer [widget]]
    [more-speech.ui.graphics :as g]))

(declare draw-article-window)

(defrecord article-window [x y w h]
  widget
  (setup-widget [widget state])
  (update-widget [widget state])
  (draw-widget [widget state]
    (draw-article-window (:application state) widget))
  (mouse-up [widget state position])
  (mouse-down [widget state position])
  )

(defn draw-article [window cursor article]
  (let [g (:graphics cursor)]
    (g/text-align g [:left])
    (g/fill g [0 0 0])
    (text/render cursor window (a/markup-article article)))
  )

(defn draw-articles [application window]
  (let [g (:graphics application)]
    (loop [cursor (text/->cursor g 0 (g/line-height g) 5)
           articles (take 20 (:articles application))]
      (if (empty? articles)
        cursor
        (recur (draw-article window cursor (first articles))
               (rest articles))))))

(defn draw-article-window [application window]
  (let [g (:graphics application)]
    (g/with-translation
      g [(:x window) (:y window)]
      (fn [g]
        (g/stroke g [0 0 0])
        (g/stroke-weight g 2)
        (g/fill g [255 255 255])
        (g/rect g [0 0 (:w window) (:h window)])
        (draw-articles application window))
      )))
