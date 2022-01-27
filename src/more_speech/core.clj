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
            ))

(def events (atom []))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  (let [
        bold (q/create-font "CourierNewPS-BoldMT" 14)
        regular (q/create-font "CourierNewPSMT" 14)
        fonts {:bold bold :regular regular}
        graphics (g/->quil-graphics fonts)
        application (map->application {:path [:application] :graphics graphics })
        application (setup-widget application {})
        state {:application application}]
    (q/text-font bold)
    (setup-child-widgets application state)
    ))

(defn update-state [{:keys [application] :as state}]
  (let [state (assoc state :application (update-widget application state))
        state (update-child-widgets (:application state) state)]
    (if (empty? @events)
      state
      (let [batch (take 10 @events)]
        (swap! events #(drop 10 %))
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

(declare more-speech)
(defn ^:export -main [& args]
  (q/defsketch more-speech
               :title "More Speech"
               :size [(q/screen-width) (q/screen-height)]
               :setup setup
               :update update-state
               :draw draw-state
               :middleware [m/fun-mode])
  (reset! events (read-string (slurp "nostr-messages")))
  args
  )


