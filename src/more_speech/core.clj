(ns more-speech.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [more-speech.nostr.events :as nostr]
            [more-speech.ui.widget :refer [draw-widget]]
            [more-speech.ui.application :refer [make-application]]
            ))

(def events (atom []))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  (let [bold (q/create-font "CourierNewPS-BoldMT" 14)
        regular (q/create-font "CourierNewPSMT" 14)]
    (q/text-font bold)
    {:application (make-application bold regular)}
    ))

(defn update-state [state]
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
            (rest batch)))))))

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


