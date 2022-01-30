(ns more-speech.ui.article-window
  (:require
    [more-speech.ui.cursor :as text]
    [more-speech.article :as a]
    [more-speech.ui.widget :refer [widget]]
    [more-speech.ui.button :refer [map->button]]
    [more-speech.ui.graphics :as g]))

(declare draw-article-window
         scroll-up
         scroll-down)

(defrecord article-window [x y w h page-up page-down display-position]
  widget
  (setup-widget [widget state]
    (assoc widget :display-position 0
                  :page-up (map->button {:x (+ x 20) :y (+ y h -20) :h 20 :w 20
                                         :left-up scroll-up})
                  :page-down (map->button {:x (+ x w -20) :y (+ y h -20) :h 20 :w 20
                                           :left-up scroll-down})
                  ))
  (update-widget [widget state]
    [widget state])
  (draw-widget [widget state]
    (draw-article-window (:application state) widget))
  )

(defn- scroll-up [button state]
  (let [button-path (:path button)
        parent-path (drop-last button-path)
        article-window (get-in state parent-path)
        article-window (update article-window :display-position + 20)
        state (assoc-in state parent-path article-window)]
    [button state]))

(defn- scroll-down [button state]
  (let [button-path (:path button)
        parent-path (drop-last button-path)
        article-window (get-in state parent-path)
        article-window (update article-window :display-position - 20)
        state (assoc-in state parent-path article-window)]
    [button state]))

(defn draw-article [window cursor article]
  (let [g (:graphics cursor)]
    (g/text-align g [:left])
    (g/fill g [0 0 0])
    (text/render cursor window (a/markup-article article)))
  )

(defn draw-articles [application window]
  (let [g (:graphics application)
        articles (:articles application)
        display-position (:display-position window)
        articles (drop display-position articles)
        articles (take 20 articles)]
    (loop [cursor (text/->cursor g 0 (g/line-height g) 5)
           articles articles]
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
