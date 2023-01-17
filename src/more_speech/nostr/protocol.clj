(ns more-speech.nostr.protocol
  (:require [clojure.core.async :as async]
            [clojure.stacktrace :as st]
            [more-speech.relay :as relay]
            [more-speech.websocket-relay :as ws-relay]
            [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.nostr.events :as events]
            [more-speech.nostr.relays :refer [relays]]
            [more-speech.nostr.util :as util]
            [more-speech.user-configuration :as user-configuration]
            [more-speech.config :as config])
  (:import (java.util Date)
           (java.text SimpleDateFormat)))

(defn format-time [time]
  (let [time (* time 1000)
        date (Date. (long time))]
    (.format (SimpleDateFormat. "MM/dd/yyyy kk:mm:ss z") date))
  )

(defn request-contact-lists [relay id]
  (let [now (quot (System/currentTimeMillis) 1000)
        days-ago config/read-contact-lists-days-ago
        seconds-ago (* days-ago 86400)
        since (int (- now seconds-ago))]
    (relay/send relay ["REQ" id {"kinds" [3] "since" since}])))

(defn request-metadata [relay id]
  (relay/send relay ["REQ" id {"kinds" [0] "since" 0}]))

(defn subscribe
  ([relay id]
   (subscribe relay id (int (- (quot (System/currentTimeMillis) 1000) 86400))))
  ([relay id since]
   (relay/send relay ["REQ" id {"since" since}])))

(defn unsubscribe [relay id]
  (relay/send relay ["CLOSE" id]))

(defn compute-id [event]
  (util/bytes->num
    (events/make-id
      {:pubkey (get event "pubkey")
       :created_at (get event "created_at")
       :kind (get event "kind")
       :tags (get event "tags")
       :content (get event "content")})))

(defn is-text-event? [event]
  (or (= (:kind event) 1)
      (= (:kind event) 4)))

(def event-counter (atom {:total 0}))
(defn count-event [envelope url]
  (let [source (second envelope)
        key (str url "|" source)]
    (swap! event-counter update :total inc)
    (swap! event-counter update key #(inc (if (nil? %) 0 %)))
    (when (zero? (mod (:total @event-counter) 1000))
      (clojure.pprint/pprint @event-counter))))

(defn record-and-display-event [_agent envelope url]
  (count-event envelope url)
  (try
    (let [[_name _subscription-id inner-event :as _decoded-msg] envelope
          event (events/translate-event inner-event)
          id (:id event)
          computed-id (compute-id inner-event)
          ui-handler (get-event-state :event-handler)
          dup? (contains? (get-event-state :text-event-map) id)]
      (if (= id computed-id)
        (let [event (events/decrypt-dm-event event)]
          (when (not (:private event))
            (swap! (:event-context @ui-context) events/process-event event url)
            (when (and (not dup?)
                       (is-text-event? event))
              (events/handle-text-event ui-handler event)
              )))
        (prn 'id-mismatch url 'computed-id (util/num32->hex-string computed-id) envelope)
        ))
    (catch Exception e
      (do (prn `record-and-display-event url (.getMessage e))
          (prn "--on event: " envelope)
          (st/print-stack-trace e))
      )))

(defn close-connection [relay id]
  (unsubscribe relay id)
  (relay/close relay))

(def event-agent (agent nil))

(defn handle-relay-message [relay message]
  (let [url (::ws-relay/url relay)]
    (send event-agent record-and-display-event message url)))

(defn connect-to-relays []
  (let [urls (if config/test-run?
               ["wss://relay.damus.io"]
               (keys @relays))]
    (doseq [url urls]
      (let [relay (ws-relay/make url handle-relay-message)
            relay-config (get @relays url)
            should-connect? (or (:read relay-config)
                                (:write relay-config))
            open-relay (if should-connect?
                         (relay/open relay)
                         nil)]
        (when (some? open-relay)
          (swap! relays assoc-in [url :connection] open-relay)))))
  (prn 'relay-connection-attempts-complete))

(defn request-contact-lists-from-relays [id]
  (prn 'requesting-contact-lists)
  (doseq [url (keys @relays)]
    (let [relay (get-in @relays [url :connection])
          read? (get-in @relays [url :read])]
      (when (and read? (some? relay))
        (unsubscribe relay id)
        (request-contact-lists relay id)))))

(defn request-metadata-from-relays [id]
  (prn 'requesting-metadata)
  (doseq [url (keys @relays)]
    (let [relay (get-in @relays [url :connection])
          read? (get-in @relays [url :read])]
      (when (and read? (some? relay))
        (unsubscribe relay id)
        (request-metadata relay id)))))

(defn subscribe-to-relays [id subscription-time]
  (let [date (- subscription-time 100)]
    (prn 'subscription-date date (format-time date))
    (doseq [url (keys @relays)]
      (let [relay (get-in @relays [url :connection])
            read? (get-in @relays [url :read])]
        (when (and read? (some? relay))
          (unsubscribe relay id)
          (subscribe relay id date)
          (swap! relays assoc-in [url :subscribed] true))))))

(defn process-send-channel []
  (let [send-chan (get-event-state :send-chan)
        urls (keys @relays)
        send-urls (filter #(:write (get @relays %)) urls)
        writeable-relays (map #(get-in @relays [% :connection]) send-urls)
        writeable-relays (filter some? writeable-relays)]
    (loop [[type msg] (async/<!! send-chan)]
      (condp = type
        :closed :quit
        :relaunch :relaunch
        :event
        (do
          (doseq [relay writeable-relays] (relay/send relay msg))
          (recur (async/<!! send-chan)))))))

(defn unsubscribe-from-relays [id]
  (doseq [url (keys @relays)]
    (let [relay (get-in @relays [url :connection])]
      (when (some? relay)
        (prn 'closing url)
        (close-connection relay id)))))

(defn get-events [subscription-time]
  (let [subscription-id "more-speech"
        metadata-request-id "more-speech-metadata"
        contact-lists-request-id "more-speech-contact-lists"
        event-handler (get-event-state :event-handler)
        now-in-seconds (quot (System/currentTimeMillis) 1000)]
    (connect-to-relays)
    (request-contact-lists-from-relays contact-lists-request-id)
    (when (user-configuration/should-import-metadata? now-in-seconds)
      (request-metadata-from-relays metadata-request-id)
      (user-configuration/set-last-time-metadata-imported now-in-seconds))
    (subscribe-to-relays subscription-id subscription-time)
    (events/update-relay-panel event-handler)
    (if (user-configuration/should-export-profile? now-in-seconds)
      (do
        (user-configuration/set-last-time-profile-exported now-in-seconds)
        (future (events/compose-and-send-metadata-event)))
      (println "Not time to export profile yet."))
    (let [exit-condition (process-send-channel)]
      (unsubscribe-from-relays subscription-id)
      (Thread/sleep 100)
      (prn 'done)
      exit-condition))
  )