(ns more-speech.ui.text-window
  (:require
    [more-speech.ui.widget :refer [widget]]
    [more-speech.ui.text-frame :refer [map->text-frame]]
    [more-speech.ui.button :refer [map->button
                                   up-arrow
                                   down-arrow
                                   draw-thumb]]
    [more-speech.ui.graphics :as g]
    [more-speech.nostr.util :refer [num->hex-string]]
    [more-speech.ui.app-util :as app]
    [more-speech.ui.config :as config]
    ))

(declare setup-text-window
         draw-text-window
         update-text-window
         drag-thumb
         lock-thumb
         unlock-thumb
         thumb-position
         scroll-up
         scroll-down)

(defrecord text-window [x y w h page-up page-down controls]
  widget
  (setup-widget [widget state]
    (setup-text-window widget state))
  (update-widget [widget state]
    (update-text-window widget state))
  (draw-widget [widget state]
    (draw-text-window state widget)))

(defn setup-text-window [window _state]
  (let [{:keys [x y w h controls]} window
        frame-path (concat (:path window) [:text-frame])
        scroll-up (partial scroll-up frame-path)
        scroll-down (partial scroll-down frame-path)
        frame (map->text-frame {:x x
                                :y y
                                :w (- w config/scroll-bar-w)
                                :h h
                                :controls controls
                                :display-position 0})
        sb-button-offset (+ (/ config/scroll-bar-w 2)
                            (/ config/scroll-bar-button-w 2))
        sb-button-x (+ x w (- sb-button-offset) 0.5)
        widget (assoc window
                 :text-frame frame
                 :scroll-down (map->button {:x sb-button-x
                                            :y (+ y config/scroll-bar-button-top-margin)
                                            :h config/scroll-bar-button-h
                                            :w config/scroll-bar-button-w
                                            :left-down scroll-down
                                            :left-held scroll-down
                                            :draw up-arrow})
                 :scroll-up (map->button {:x sb-button-x
                                          :y (+ y h (- config/scroll-bar-button-bottom-margin))
                                          :h config/scroll-bar-button-h
                                          :w config/scroll-bar-button-w
                                          :left-down scroll-up
                                          :left-held scroll-up
                                          :draw down-arrow})
                 :thumb (map->button {:x sb-button-x
                                      :y (thumb-position frame)
                                      :h config/thumb-h
                                      :w config/scroll-bar-button-w
                                      :draw draw-thumb
                                      :left-held drag-thumb
                                      :left-down lock-thumb
                                      :left-up unlock-thumb
                                      }))]
    widget))

(defn update-text-window [widget state]
  (let [text-frame (:text-frame widget)
        thumb-pos (thumb-position text-frame)
        thumb-path (concat (:path widget) [:thumb])]
    (assoc-in state (concat thumb-path [:y]) thumb-pos)))

(defn draw-text-window [state window]
  (let [application (:application state)
        g (:graphics application)]
    (g/with-translation
      g [(:x window) (:y window)]
      (fn [g]
        (g/stroke g [0 0 0])
        (g/stroke-weight g 2)
        (g/fill g config/white)
        (g/rect g [0 0 (:w window) (:h window)])))))

(defn scroll-up [frame-path _button state]
  (let [frame (get-in state frame-path)]
    ((:scroll-frame frame) frame-path state 1)))

(defn scroll-down [frame-path _button state]
  (let [frame (get-in state frame-path)]
    ((:scroll-frame frame) frame-path state -1)))

(defn- thumb-drag-height [frame]
  (- (:h frame)
     (* 2 (+ config/scroll-bar-button-top-margin
             config/scroll-bar-button-h
             config/thumb-margin))
     config/thumb-h))

(defn- thumb-origin [frame]
  (+ (:y frame)
     config/scroll-bar-button-top-margin
     config/scroll-bar-button-h
     config/thumb-margin))

(defn- thumb-position [text-frame]
  (let [display-position (get text-frame :display-position 0)
        total-elements (get text-frame :total-elements 0)
        height (thumb-drag-height text-frame)]
    (if (zero? total-elements)
      (thumb-origin text-frame)
      (+ (thumb-origin text-frame)
         (* height (/ display-position total-elements))))))

(defn- drag-thumb [button state]
  (let [graphics (get-in state [:application :graphics])
        thumb-path (:path button)
        parent-path (drop-last thumb-path)
        text-window-path (drop-last thumb-path)
        text-frame-path (concat text-window-path [:text-frame])
        text-frame (get-in state text-frame-path)
        total-elements (get text-frame :total-elements 0)
        height (thumb-drag-height text-frame)
        top (thumb-origin text-frame)
        [_ my _] (g/get-mouse graphics)
        dy (- my top)
        dy (max dy 0)
        dy (min dy height)
        display-position (* (/ dy height) total-elements)
        state (assoc-in state
                        (concat text-frame-path [:display-position])
                        display-position)]
    (app/update-widget state parent-path)))

(defn- lock-thumb [widget state]
  (app/lock-mouse state widget))

(defn- unlock-thumb [_widget state]
  (app/unlock-mouse state))
