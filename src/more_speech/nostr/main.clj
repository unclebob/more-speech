(ns more-speech.nostr.main
  (:require [more-speech.nostr.protocol :as protocol]
            [more-speech.ui.swing.ui-context :as context]
            [more-speech.user-configuration :as user-configuration]
            [more-speech.nostr.events :as events]
            [more-speech.nostr.event-composers :as composers]
            [more-speech.nostr.relays :as relays]
            [more-speech.relay :as relay]
            [clojure.core.async :as async]))

(defn process-send-channel []
  (let [send-chan (context/get-event-state :send-chan)
        urls (keys @relays/relays)
        send-urls (filter #(:write (get @relays/relays %)) urls)
        writeable-relays (map #(get-in @relays/relays [% :connection]) send-urls)
        writeable-relays (filter some? writeable-relays)]
    (loop [[type msg] (async/<!! send-chan)]
      (condp = type
        :closed :quit
        :relaunch :relaunch
        :event
        (do
          (doseq [relay writeable-relays] (relay/send relay msg))
          (recur (async/<!! send-chan)))))))

(defn start-nostr [subscription-time]
  (let [subscription-id "more-speech"
        metadata-request-id "more-speech-metadata"
        contact-lists-request-id "more-speech-contact-lists"
        event-handler (context/get-event-state :event-handler)
        now-in-seconds (quot (System/currentTimeMillis) 1000)]
    (protocol/connect-to-relays)
    (protocol/request-contact-lists-from-relays contact-lists-request-id)
    (when (user-configuration/should-import-metadata? now-in-seconds)
      (protocol/request-metadata-from-relays metadata-request-id)
      (user-configuration/set-last-time-metadata-imported now-in-seconds))
    (protocol/subscribe-to-relays subscription-id subscription-time)
    (events/update-relay-panel event-handler)
    (if (user-configuration/should-export-profile? now-in-seconds)
      (do
        (user-configuration/set-last-time-profile-exported now-in-seconds)
        (future (composers/compose-and-send-metadata-event)))
      (println "Not time to export profile yet."))
    (let [exit-condition (process-send-channel)]
      (protocol/unsubscribe-from-relays subscription-id)
      (Thread/sleep 100)
      (prn 'done)
      exit-condition)))