(ns more-speech.nostr.main
  (:require [clojure.core.async :as async]
            [more-speech.config :as config]
            [more-speech.logger.default :refer [log log-pr]]
            [more-speech.mem :refer :all]
            [more-speech.nostr.event-composers :as composers]
            [more-speech.nostr.protocol :as protocol]
            [more-speech.nostr.util :as util]
            [more-speech.relay :as relay]
            [more-speech.user-configuration :as user-configuration]))

(defn send-event-to-relays [msg]
  (let [relays (get-mem :relays)
        urls (keys relays)
        send-urls (filter #(:write (get relays %)) urls)
        writeable-relays (map #(get-in relays [% :connection]) send-urls)
        writeable-relays (filter some? writeable-relays)]
    (doseq [relay writeable-relays] (relay/send relay msg))))

(defn process-send-channel []
  (let [send-chan (get-mem :send-chan)]
    (loop [[type msg] (async/<!! send-chan)]
      (condp = type
        :closed :quit
        :relaunch :relaunch
        :event (do (send-event-to-relays msg)
                   (recur (async/<!! send-chan)))))))

(defn start-nostr [subscription-time]
  (protocol/initialize)
  (let [now-in-seconds (util/get-now)]
    (protocol/connect-to-relays)
    (when (user-configuration/should-import-metadata? now-in-seconds)
      (protocol/request-metadata-from-relays (- now-in-seconds 86400))
      (user-configuration/set-last-time-metadata-imported now-in-seconds))
    (protocol/subscribe-to-relays subscription-time now-in-seconds)
    (when (and config/read-contacts (not (config/is-test-run?)))
      (protocol/request-contact-lists-from-relays))
    (if (user-configuration/should-export-profile? now-in-seconds)
      (do
        (user-configuration/set-last-time-profile-exported now-in-seconds)
        (future (composers/compose-and-send-metadata-and-relay-recommendations)))
      (log 2 "Not time to export profile yet."))
    (let [exit-condition (process-send-channel)]
      (protocol/close-all-relays)
      (Thread/sleep 100)
      (log-pr 2 'done)
      exit-condition)))