(ns more-speech.ui.swing.stats-window
  (:require [more-speech.mem :refer :all]
            [more-speech.logger.default :refer :all])
   (:use [seesaw core])
   (:import (java.util Timer TimerTask)))


(defn show-kinds [stats-panel]
  (doseq [kind (keys (get-mem [:event-counter :kinds]))]
    (let [id (keyword (str "#kind-" kind))]
      (config! (select stats-panel [id])
               :text (str (get-mem [:event-counter :kinds kind]))))))

(defn show-status [stats-panel]
  (config! (select stats-panel [:#backlog-data])
           :text (str (get-mem :websocket-backlog)))
  (config! (select stats-panel [:#processed-data])
           :text (str (get-mem [:event-counter :total])))
  (config! (select stats-panel [:#incoming-data])
           :text (str (get-mem [:incoming-events])))
  (config! (select stats-panel [:#dups-data])
           :text (str (get-mem [:event-counter :dups])))
  (show-kinds stats-panel))

(defn close-stats-frame [timer menu _e]
  (config! menu :enabled? true)
  (.cancel timer))

(defn make-stat-panel [name id]
  (let [stat-label (label name)
        stat-data (label :text "" :id id :size [100 :by 20])
        stat-panel (left-right-split stat-data stat-label)]
    stat-panel))

(defn make-kind-panels []
  (loop [kinds (sort (keys (get-mem [:event-counter :kinds])))
         kind-panels []]
    (if (empty? kinds)
      kind-panels
      (let [kind (first kinds)
            kind-panel (make-stat-panel (str "Kind:" kind)
                                        (keyword (str "kind-" kind)))]
        (recur (rest kinds) (conj kind-panels kind-panel)))))
  )

(defn make-stats-frame [_e]
  (let [stats-frame (frame :title (str "Stats - " @log-level) )
        incoming-panel (make-stat-panel "Incoming events." :incoming-data)
        backlog-panel (make-stat-panel "Backlog." :backlog-data)
        processed-panel (make-stat-panel "Processed events." :processed-data)
        dups-panel (make-stat-panel "Duplicate events." :dups-data)
        kind-panels (make-kind-panels)

        stats-panel (vertical-panel :items (concat [incoming-panel
                                                    processed-panel
                                                    backlog-panel
                                                    dups-panel]
                                                   kind-panels))

        stats-timer (Timer. "stats timer")
        show-status-task (proxy [TimerTask] []
                           (run [] (show-status stats-panel)))

        stats-menu (select (get-mem :frame) [:#stats-menu])]
    (config! stats-frame :content stats-panel)
    (config! stats-menu :enabled? false)
    (listen stats-frame :window-closing (partial close-stats-frame stats-timer stats-menu))
    (pack! stats-frame)
    (show! stats-frame)
    (.schedule stats-timer show-status-task 1000 1000)))
