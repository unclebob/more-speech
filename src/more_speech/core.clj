;;Notes:
;; Nice debug site: https://nostr-army-knife.netlify.app

(ns more-speech.core
  (:gen-class)
  (:require [clojure.core.async :as async]
            [more-speech.config :as config]
            [more-speech.data-storage :as data-storage]
            [more-speech.logger.default :refer [log-level log-pr]]
            [more-speech.mem :refer :all]
            [more-speech.migrator :as migrator]
            [more-speech.nostr.main :as main]
            [more-speech.nostr.util :as util]
            [more-speech.ui.formatter-util :as formatter-util]
            [more-speech.ui.swing.main-window :as swing])
  (:use (seesaw [core])))

(def send-chan (async/chan))

(defn ^:export -main [& args]
  (let [arg (first args)]
    (when (= "compress" arg)
      (data-storage/compress)
      (Thread/sleep 1000)
      (System/exit 1))
    (log-pr 1 'main arg 'start)
    (when (= "test" arg)
      (config/test-run!)
      (reset! log-level 2))
    (when (= "dev" arg)
      (reset! log-level 2))
    (when (and (some? arg) (re-matches #"hours:\d+" arg))
      (let [hours (Integer/parseInt (subs arg 6))]
        (set-mem :request-hours-ago hours)))
    (when (and (some? arg) (re-matches #"dev-hours:\d+" arg))
      (reset! log-level 2)
      (let [hours (Integer/parseInt (subs arg 10))]
        (set-mem :request-hours-ago hours)
        (log-pr 2 'hours-ago hours)))
    (if (config/is-test-run?)
      (config/set-db! :in-memory)
      (config/set-db! config/production-db))
    (migrator/migrate-to config/migration-level)
    (log-pr 2 'main 'loading-configuration)
    (data-storage/load-configuration)
    (log-pr 2 'main 'setting-up-gui)
    (let [handler (swing/setup-main-window)]
      (log-pr 2 'main 'main-window-setup-complete)
      (set-mem :send-chan send-chan)
      (set-mem :event-handler handler)
      (log-pr 2 'main 'reading-in-last-n-days)
      (let [latest-old-message-time
            (if (not (config/is-test-run?))
              (data-storage/load-event-history handler)
              (-> (util/get-now) (- 7200)))
            latest-old-message-time
            (if (some? (get-mem :request-hours-ago))
              (- (util/get-now) (* 3600 (get-mem :request-hours-ago)))
              latest-old-message-time)
            _ (log-pr 2 'main 'getting-events (formatter-util/format-time latest-old-message-time))
            exit-condition (main/start-nostr latest-old-message-time)]
        (log-pr 2 'starting-exit-process)
        (data-storage/write-configuration)
        (if (= exit-condition :relaunch)
          (do
            (invoke-now (.dispose (get-mem :frame)))
            (recur args)
            )
          (System/exit 1))))))



