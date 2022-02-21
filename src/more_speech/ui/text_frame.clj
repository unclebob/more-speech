(ns more-speech.ui.text-frame
  (:require
    [more-speech.ui.widget :refer [widget]]
    [more-speech.ui.graphics :as g]
    [more-speech.content.article :as a]
    [more-speech.ui.app-util :as app-util]
    [more-speech.ui.header-frame-functions
     :refer [get-element-height
             draw-headers]
     :as funcs]
    ))

(declare setup-text-frame
         update-text-frame
         draw-text-frame
         mouse-wheel
         scroll-frame)

(defrecord text-frame [x y w h display-position]
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
  (let [element-height (get-element-height state)
        elements (quot (:h frame) element-height)
        frame (assoc frame :n-elements elements
                           :mouse-wheel mouse-wheel
                           :scroll-frame scroll-frame)]
    frame))

(defn- update-text-frame [state frame]
  (if (app-util/update-widget? state frame)
    (let [application (:application state)
          event-map (:text-event-map application)
          events (:chronological-text-events application)
          open-thread (:open-thread application)
          threaded-events (a/thread-events events event-map open-thread)
          total-events (count threaded-events)
          display-position (:display-position frame)
          end-position (min (count threaded-events) (+ display-position (:n-elements frame)))
          displayed-events (subvec threaded-events display-position end-position)
          nicknames (:nicknames application)
          headers (funcs/events->headers displayed-events nicknames)
          bc (funcs/make-button-creator state frame)
          frame-path (:path frame)
          frame (get-in state frame-path)
          frame (app-util/clear-widgets frame)
          buttons (funcs/create-thread-buttons bc headers)
          frame (funcs/add-thread-buttons frame buttons)
          marked-up-headers (map a/markup-header headers)
          frame (assoc frame :displayed-elements marked-up-headers
                             :total-elements total-events)
          state (assoc-in state frame-path frame)]
      state)
    state))

(defn draw-text-frame [state frame]
  (let [{:keys [x y w h]} frame
        application (:application state)
        g (:graphics application)]
    (g/with-translation
      g [x y]
      (fn [g]
        (g/stroke g [0 0 0])
        (g/stroke-weight g 2)
        (g/no-fill g)
        (g/rect g [0 0 w h])
        (draw-headers state frame)))))

(defn scroll-frame [frame-path state delta]
  (let [frame (get-in state frame-path)
        articles (get-in state [:application :chronological-text-events])
        display-position (:display-position frame)
        display-position (+ display-position delta)
        display-position (min (count articles) display-position)
        display-position (max 0 display-position)
        frame (assoc frame :display-position display-position)
        state (assoc-in state frame-path frame)
        state (app-util/update-widget state frame)]
    state))

(defn mouse-wheel [widget state clicks]
  (scroll-frame (:path widget) state clicks))

