;;Notes:
;; Nice debug site: https://nostr-army-knife.netlify.app

(ns more-speech.core
  (:gen-class)
  (:require [more-speech.config :as config]
            [more-speech.nostr.protocol :as protocol]
            [more-speech.ui.swing.main-window :as swing]
            [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.migrator :as migrator]
            [more-speech.data-storage :as data-storage]
            [clojure.core.async :as async])
  (:use [seesaw core])
  )

(def send-chan (async/chan))

(declare set-event-handler)

(defn ^:export -main [& args]
  (migrator/migrate-to config/migration-level)
  (data-storage/load-configuration)
  (let [event-context (:event-context @ui-context)
        handler (swing/setup-main-window)]
    (swap! event-context set-event-handler handler)
    (swap! event-context assoc :send-chan send-chan)
    (let [latest-old-message-time
          (if (not config/test-run?)
            (data-storage/read-in-last-n-days config/days-to-read event-context handler)
            (-> (System/currentTimeMillis) (quot 1000) (- 86400)))
          exit-condition (protocol/get-events event-context latest-old-message-time)]

      (when (not config/test-run?)
        (data-storage/write-configuration)
        (data-storage/write-changed-days event-context))
      (if (= exit-condition :relaunch)
        (do
          (invoke-now (.dispose (:frame @ui-context)))
          (recur args)
          )
        (System/exit 1)))))

(defn set-event-handler [event-state handler]
  (assoc event-state :event-handler handler))



