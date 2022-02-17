(ns more-speech.ui.article-window
  (:require
    [more-speech.ui.widget :refer [widget]]
    [more-speech.ui.button :refer [map->button
                                   up-arrow
                                   down-arrow
                                   thumb]]
    [more-speech.ui.graphics :as g]
    [more-speech.nostr.util :refer [num->hex-string]]
    [more-speech.ui.header-frame :refer [scroll-up
                                         scroll-down
                                         map->header-frame]]
    ))

(declare draw-article-window
         update-article-window)

(defn- thumb-position [header-frame]
  (let [display-position (get header-frame :display-position 0)
        total-headers (get header-frame :total-headers 0)
        height (- (:h header-frame) 65)]
    (if (zero? total-headers)
      0
      (+ (:y header-frame) 25
         (* height (/ display-position total-headers))))))

(defrecord article-window [x y w h page-up page-down]
  widget
  (setup-widget [widget state]
    (let [frame-path (conj (:path widget) :header-frame)
          scroll-up (partial scroll-up frame-path)
          scroll-down (partial scroll-down frame-path)
          frame (map->header-frame {:x (inc x)
                                    :y (inc y)
                                    :w (- w 20)
                                    :h (dec h)
                                    :display-position 0})]
      (assoc widget
        :header-frame frame
        :page-up (map->button {:x (+ x w -17) :y (+ y 5) :h 15 :w 15
                               :left-down scroll-down
                               :left-held scroll-down
                               :draw up-arrow})
        :page-down (map->button {:x (+ x w -17) :y (+ y h -20) :h 15 :w 15
                                 :left-down scroll-up
                                 :left-held scroll-up
                                 :draw down-arrow})
        :thumb (map->button {:x (+ x w -17) :y (thumb-position frame) :h 15 :w 15
                             :draw thumb}))))

  (update-widget [widget state]
    (update-article-window widget state))
  (draw-widget [widget state]
    (draw-article-window state widget)))

(defn update-article-window [widget state]
  (let [header-frame (:header-frame widget)
        thumb-pos (thumb-position header-frame)
        thumb-path (conj (:path widget) :thumb)]
    (assoc-in state (conj thumb-path :y) thumb-pos)))

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
