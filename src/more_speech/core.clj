(ns more-speech.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [more-speech.nostr.events :as nostr]
            [more-speech.ui.widget :refer [draw-widget
                                           setup-widget
                                           update-widget
                                           setup-child-widgets
                                           update-child-widgets]]
            [more-speech.ui.application :refer [map->application]]
            [more-speech.ui.graphics :as g]
            [more-speech.ui.widget :as w]))

(def events (atom []))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  (let [
        bold (q/create-font "CourierNewPS-BoldMT" 14)
        regular (q/create-font "CourierNewPSMT" 14)
        fonts {:bold bold :regular regular}
        graphics (g/->quil-graphics fonts)
        application (map->application {:path [:application] :graphics graphics})
        application (setup-widget application {})
        state {:application application}]
    (q/text-font bold)
    (setup-child-widgets application state)
    ))

(defn update-state [{:keys [application] :as state}]
  (let [state (update-widget application state)
        state (update-child-widgets (:application state) state)
        next-update (get-in state [:application :next-update])
        state (assoc-in state [:application :this-update] next-update)
        state (assoc-in state [:application :next-update] #{})]
    (if (empty? @events)
      state
      (let [n-events (count @events)
            batch-size (min n-events 100)
            batch (take batch-size @events)]
        (swap! events #(drop batch-size %))
        (loop [state state
               batch batch]
          (if (empty? batch)
            state
            (recur
              (nostr/process-event state (first batch))
              (rest batch))))))))

(defn draw-state [{:keys [application] :as state}]
  (q/background 240 240 240)
  (draw-widget application state)
  )

(defn mouse-wheel [state delta]
  (let [application (:application state)
        x (q/mouse-x)
        y (q/mouse-y)
        widget (w/find-deepest-mouse-target application x y)
        wheel-f (get widget :mouse-wheel)]
    (if (some? wheel-f)
      (wheel-f widget state delta)
      state))
  )

(declare more-speech)
(defn ^:export -main [& args]
  (q/defsketch more-speech
               :title "More Speech"
               :size [(q/screen-width) (q/screen-height)]
               :setup setup
               :update update-state
               :draw draw-state
               :mouse-wheel mouse-wheel
               :middleware [m/fun-mode])
  (reset! events (read-string (slurp "nostr-messages")))
  args
  )


