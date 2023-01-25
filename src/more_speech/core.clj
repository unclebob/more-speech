;;Notes:
;; Nice debug site: https://nostr-army-knife.netlify.app

(ns more-speech.core
  (:gen-class)
  (:require [more-speech.config :as config]
            [more-speech.nostr.main :as main]
            [more-speech.ui.swing.main-window :as swing]
            [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.migrator :as migrator]
            [more-speech.data-storage :as data-storage]
            [clojure.core.async :as async])
  (:use [seesaw core]))

(def send-chan (async/chan))

(defn ^:export -main [& args]
  (prn 'main 'start)
  (migrator/migrate-to config/migration-level)
  (prn 'main 'loading-configuration)
  (data-storage/load-configuration)
  (prn 'main 'setting-up-gui)
  (let [event-context (:event-context @ui-context)
        handler (swing/setup-main-window)]
    (prn 'main 'main-window-setup-complete)
    (swap! event-context assoc :send-chan send-chan :event-handler handler)
    (prn 'main 'reading-in-last-n-days)
    (let [latest-old-message-time
          (if (not config/test-run?)
            (data-storage/read-in-last-n-days config/days-to-read handler)
            (-> (System/currentTimeMillis) (quot 1000) (- 3600)))
          _ (prn 'main 'getting-events)
          exit-condition (main/start-nostr latest-old-message-time)]
      (prn 'starting-exit-process)
      (when (not config/test-run?)
        (data-storage/write-configuration)
        (data-storage/write-changed-days))
      (if (= exit-condition :relaunch)
        (do
          (invoke-now (.dispose (:frame @ui-context)))
          (recur args)
          )
        (System/exit 1)))))



