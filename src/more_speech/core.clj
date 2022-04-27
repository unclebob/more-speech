;;Stories
;; - Add author/date, etc. to replies.
;; - Start checking sdefs in update.
;; - Clean up java schnorr library.
;; - Threading does not work quite right.  Do some diagnosis.
;; - Reply
;; - Mark read and highlight properly.
;; - Save names and headers.  Request after latest save.
;; - Click, double-click, drag in edit window.
;; - Consider subject/topic in the tags

;;Notes:
;; Nice debug site: https://nostr-army-knife.netlify.app


(ns more-speech.core
  (:require [quil.core :as q]
            [clojure.core.async :as async]
            [more-speech.nostr.events :as nostr]
            [more-speech.ui.widget :refer [draw-widget
                                           setup-widget
                                           update-widget
                                           setup-child-widgets
                                           update-child-widgets]]
            [more-speech.ui.application :refer [map->application]]
            [more-speech.ui.graphics :as g]
            [more-speech.ui.widget :as w]
            [more-speech.nostr.protocol :as protocol]
            [more-speech.ui.swing.main-window :as swing]
            [more-speech.nostr.events :as events])
  )

(def events (atom []))
(def send-chan (async/chan))

(defn get-keys [state]
  (let [keys (read-string (slurp "private/keys"))
        state (assoc state :keys keys)]
    state))

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
        state {:application application}
        state (get-keys state)
        state (assoc state :send-chan send-chan)]
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
            batch-size (min n-events 20)
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
  (w/set-cursor application)
  (draw-widget application state)
  )

(declare more-speech setup-jframe)
(defn ^:export -main [& args]
  ;(q/defsketch more-speech
  ;             :title "More Speech"
  ;             :size [(q/screen-width) (- (q/screen-height) config/window-margin)]
  ;             :setup setup
  ;             :update update-state
  ;             :draw draw-state
  ;             :mouse-wheel w/mouse-wheel
  ;             :mouse-pressed w/mouse-pressed
  ;             :mouse-released w/mouse-released
  ;             :mouse-moved w/mouse-moved
  ;             :mouse-dragged w/mouse-dragged
  ;             :key-pressed w/key-pressed
  ;             :middleware [m/fun-mode]
  ;             :on-close protocol/close-connection)

  ;(reset! events (read-string (slurp "nostr-messages")))
  (let [event-agent (events/make-event-agent)]
    (swing/setup-jframe event-agent send-chan)
    (protocol/get-events event-agent send-chan))
  args
  )



