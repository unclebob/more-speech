(ns more-speech.ui.article-window
  (:require
    [more-speech.ui.widget :refer [widget]]
    [more-speech.ui.button :refer [map->button
                                   up-arrow
                                   down-arrow]]
    [more-speech.ui.graphics :as g]
    [more-speech.nostr.util :refer [num->hex-string]]
    [more-speech.ui.header-frame :refer [scroll-up
                                         scroll-down
                                         map->header-frame]]
    ))

(declare draw-article-window)

(defrecord article-window [x y w h page-up page-down]
  widget
  (setup-widget [widget state]
    (let [frame-path (conj (:path widget) :header-frame)
          scroll-up (partial scroll-up frame-path)
          scroll-down (partial scroll-down frame-path)]
      (assoc widget
        :header-frame (map->header-frame {:x (inc x)
                                          :y (inc y)
                                          :w (- w 30)
                                          :h (dec h)
                                          :display-position 0})
        :page-up (map->button {:x (+ x w -20) :y (+ y 20) :h 20 :w 20
                               :left-down scroll-down
                               :left-held scroll-down
                               :draw up-arrow})
        :page-down (map->button {:x (+ x w -20) :y (+ y h -30) :h 20 :w 20
                                 :left-down scroll-up
                                 :left-held scroll-up
                                 :draw down-arrow}))))

  (update-widget [widget state]
    state)
  (draw-widget [widget state]
    (draw-article-window state widget)))

(defn draw-article-window [state window]
  (let [application (:application state)
        g (:graphics application)]
    (g/with-translation
      g [(:x window) (:y window)]
      (fn [g]
        (g/stroke g [0 0 0])
        (g/stroke-weight g 2)
        (g/fill g [255 255 255])
        (g/rect g [0 0 (:w window) (:h window)])))))
