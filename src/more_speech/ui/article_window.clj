(ns more-speech.ui.article-window
  (:require
    [quil.core :as q]
    [more-speech.ui.text :as text]
    [more-speech.article :as a]
    [more-speech.ui.widget :refer [widget]]))

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

(defn draw-article [window cursor article]
  (q/text-align :left)
  (q/fill 0 0 0)
  (text/render cursor window (a/markup-article article))
  )

(defn draw-articles [application {:keys [fonts] :as window}]
  (loop [cursor (text/->cursor 0 (text/line-height) 5)
         articles (take 20 (:articles application))]
    (if (empty? articles)
      cursor
      (recur (draw-article window cursor (first articles))
             (rest articles)))))

(defn draw-article-window [application window]
  (q/with-translation
    [(:x window) (:y window)]
    (q/stroke 0 0 0)
    (q/stroke-weight 2)
    (q/fill 255 255 255)
    (q/rect 0 0 (:w window) (:h window))
    (draw-articles application window)
    ))
