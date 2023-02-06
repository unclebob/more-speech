(ns more-speech.nostr.protocol
  (:require [more-speech.relay :as relay]
            [more-speech.websocket-relay :as ws-relay]
            [more-speech.mem :refer :all]
            [more-speech.nostr.events :as events]
            [more-speech.nostr.event-handlers :as handlers]
            [more-speech.nostr.relays :refer [relays]]
            [more-speech.config :as config])
  (:import (java.util Date)
           (java.text SimpleDateFormat)))

(defn- format-time [time]
  (let [time (* time 1000)
        date (Date. (long time))]
    (.format (SimpleDateFormat. "MM/dd/yyyy kk:mm:ss z") date))
  )

(defn request-contact-lists [relay id]
  (let [now (quot (System/currentTimeMillis) 1000)
        days-ago config/read-contact-lists-days-ago
        seconds-ago (int (* days-ago 86400))
        since (int (- now seconds-ago))]
    (relay/send relay ["REQ" id {"kinds" [3] "since" since}])))

(defn request-metadata [relay id since]
  (relay/send relay ["REQ" id {"kinds" [0] "since" since}]))

(defn subscribe
  ([relay id]
   (let [now (int (quot (System/currentTimeMillis) 1000))]
     (subscribe relay id (- now 86400) now)))
  ([relay id since now]
   (relay/send relay ["REQ" id {"since" since "until" now}])
   (relay/send relay ["REQ" (str id "-now") {"since" now}])))

(defn unsubscribe [relay id]
  (relay/send relay ["CLOSE" id]))

(defn close-connection [relay id]
  (unsubscribe relay id)
  (relay/close relay))

(defn handle-relay-message [relay message]
  (let [url (::ws-relay/url relay)]
    (swap! config/websocket-backlog inc)
    (send-off events/event-agent handlers/handle-event message url)))

(defn connect-to-relays []
  (let [urls (if (config/is-test-run?)
               [config/test-relay]
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

(defn request-metadata-from-relays [id since]
  (prn 'requesting-metadata)
  (doseq [url (keys @relays)]
    (let [relay (get-in @relays [url :connection])
          read? (get-in @relays [url :read])]
      (when (and read? (some? relay))
        (unsubscribe relay id)
        (request-metadata relay id since)))))

(defn subscribe-to-relays [id subscription-time now]
  (let [date (- subscription-time 100)]
    (prn 'subscription-date date (format-time date))
    (doseq [url (keys @relays)]
      (let [relay (get-in @relays [url :connection])
            read? (get-in @relays [url :read])]
        (when (and read? (some? relay))
          (unsubscribe relay id)
          (subscribe relay id date now)
          (swap! relays assoc-in [url :subscribed] true))))))

(defn unsubscribe-from-relays [id]
  (doseq [url (keys @relays)]
    (let [relay (get-in @relays [url :connection])]
      (when (some? relay)
        (prn 'closing url)
        (close-connection relay id)))))

