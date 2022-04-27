(ns more-speech.ui.swing.main-window
  (:require [clojure.core.async :as async]
            [more-speech.nostr.util :as util]
            [more-speech.ui.formatters :as formatters]
            [more-speech.nostr.events :as events]
            [clojure.string :as string])
  (:use [seesaw core font])
  (:import (javax.swing SwingUtilities Timer)))

(declare display-jframe action-event draw-events)

(defn setup-jframe [event-agent output-channel]
  (SwingUtilities/invokeLater #(display-jframe event-agent output-channel)))

(defn display-jframe [event-agent output-channel]
  (let [frame (frame :title "More Speech" :size [1000 :by 1000])
        text-area (text :multi-line? true :font "MONOSPACED-PLAIN-14")
        timer (Timer. 100 nil)]
    (listen frame :window-closing (fn [_]
                                    (.stop timer)
                                    (async/>!! output-channel [:closed])
                                    (.dispose frame)))

    (listen timer :action (fn [_] (draw-events text-area event-agent)))
    (config! frame :content (scrollable text-area))
    (show! frame)
    (.start timer)))

(declare format-events append-event format-event)

(defn draw-events [text-area event-agent]
  (prn "tick")
  (when (:update @event-agent)
    (let [event-state @event-agent
          formatted-events (format-events event-state)]
      (text! text-area formatted-events)
      (send event-agent events/updated))))

(defn format-events [{:keys [chronological-text-events nicknames text-event-map]}]
  (let [header-ids (map first chronological-text-events)
        headers (map #(get text-event-map %) header-ids)]
    (reduce #(append-event nicknames %1 %2) "" headers)))

(defn append-event [nicknames formatted-events event]
  (str formatted-events (format-event nicknames event)))

(defn format-event [nicknames {:keys [pubkey created-at content]}]
  (let [name (get nicknames pubkey (util/num->hex-string pubkey))
        name (formatters/abbreviate name 20)
        time (formatters/format-time created-at)
        content (string/replace content \newline \~)
        content (formatters/abbreviate content 50)]
    (format "%20s %s %s\n" name time content)))