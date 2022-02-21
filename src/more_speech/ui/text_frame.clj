(ns more-speech.ui.text-frame
  (:require
    [more-speech.ui.widget :refer [widget]]
    [more-speech.ui.graphics :as g]
    [more-speech.ui.app-util :as app-util]
    ))

(defprotocol text-window-controls
  (get-element-height [controls state]
    "returns the fixed height, in pixels, of the scrolled element.")
  (draw-elements [controls state frame]
    "draws :displayed-elements.")
  (update-elements [controls state frame]
    "Called only if the widget is in [:application :this-update]
    sets :total-elements and :displayed-elements")
  (scroll-elements [controls state frame delta]
    "sets :display-position"))

(declare setup-text-frame
         update-text-frame
         draw-text-frame
         mouse-wheel
         scroll-frame)

(defrecord text-frame [x y w h display-position controls]
  widget
  (setup-widget [widget state]
    (setup-text-frame state widget))
  (update-widget [widget state]
    (update-text-frame state widget))
  (draw-widget [widget state]
    (draw-text-frame state widget)
    state)
  )

(defn setup-text-frame [state frame]
  (let [controls (:controls frame)
        element-height (get-element-height controls state)
        elements (quot (:h frame) element-height)
        frame (assoc frame :n-elements elements
                           :mouse-wheel mouse-wheel
                           :scroll-frame scroll-frame)]
    frame))

(defn- update-text-frame [state frame]
  (if (app-util/update-widget? state frame)
    (update-elements (:controls frame) state frame)
    state))

(defn draw-text-frame [state frame]
  (let [{:keys [x y w h controls]} frame
        application (:application state)
        g (:graphics application)]
    (g/with-translation
      g [x y]
      (fn [g]
        (g/stroke g [0 0 0])
        (g/stroke-weight g 2)
        (g/no-fill g)
        (g/rect g [0 0 w h])
        (draw-elements controls state frame)))))

(defn scroll-frame [frame-path state delta]
  (let [frame (get-in state frame-path)
        controls (:controls frame)
        state (assoc-in state frame-path (scroll-elements controls state frame delta))
        state (app-util/update-widget state frame)]
    state))

(defn mouse-wheel [frame state clicks]
  (scroll-frame  (:path frame) state clicks))

