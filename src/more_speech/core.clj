;;Notes:
;; Nice debug site: https://nostr-army-knife.netlify.app

(ns more-speech.core
  (:gen-class)
  (:require [more-speech.config :as config]
            [more-speech.nostr.main :as main]
            [more-speech.ui.swing.main-window :as swing]
            [more-speech.mem :refer :all]
            [more-speech.migrator :as migrator]
            [more-speech.data-storage :as data-storage]
            [clojure.core.async :as async])
  (:use [seesaw core]))

(def send-chan (async/chan))

(defn ^:export -main [& args]
  (prn 'main (first args) 'start)
  (when (= "test" (first args))
    (config/test-run!))
  (if (config/is-test-run?)
    (config/set-db! :in-memory)
    (config/set-db! config/production-db))
  (migrator/migrate-to config/migration-level)
  (prn 'main 'loading-configuration)
  (data-storage/load-configuration)
  (prn 'main 'setting-up-gui)
  (let [handler (swing/setup-main-window)]
    (prn 'main 'main-window-setup-complete)
    (set-mem :send-chan send-chan)
    (set-mem :event-handler handler)
    (prn 'main 'reading-in-last-n-days)
    (let [latest-old-message-time
          (if (not (config/is-test-run?))
            (data-storage/read-in-last-n-days config/days-to-read handler)
            (-> (System/currentTimeMillis) (quot 1000) (- 3600)))
          _ (prn 'main 'getting-events)
          exit-condition (main/start-nostr latest-old-message-time)]
      (prn 'starting-exit-process)
      (when (not (config/is-test-run?))
        (data-storage/write-configuration))
      (if (= exit-condition :relaunch)
        (do
          (invoke-now (.dispose (get-mem :frame)))
          (recur args)
          )
        (System/exit 1)))))



