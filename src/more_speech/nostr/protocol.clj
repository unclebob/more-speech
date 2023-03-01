(ns more-speech.nostr.protocol
  (:require [more-speech.relay :as relay]
            [more-speech.websocket-relay :as ws-relay]
            [more-speech.mem :refer :all]
            [more-speech.nostr.events :as events]
            [more-speech.nostr.event-handlers :as handlers]
            [more-speech.mem :refer :all]
            [more-speech.config :as config]
            [more-speech.nostr.contact-list :as contact-list]
            [more-speech.nostr.util :as util])
  (:import (java.util Date)
           (java.text SimpleDateFormat)))

(defn is-active-url? [url]
  (let [relay-descriptor (get @relays url)
        read-state (:read relay-descriptor)
        write-state (:write relay-descriptor)
        is-reading? (or (= :read-all read-state)
                        (= :read-trusted read-state)
                        (= :read-web-of-trust read-state))
        is-writing? write-state]
    (or is-reading? is-writing?)))

(defn- format-time [time]
  (let [time (* time 1000)
        date (Date. (long time))]
    (.format (SimpleDateFormat. "MM/dd/yyyy kk:mm:ss z") date)))

(defn request-contact-lists [relay id]
  (let [now (quot (System/currentTimeMillis) 1000)
        days-ago config/read-contact-lists-days-ago
        seconds-ago (int (* days-ago 86400))
        since (int (- now seconds-ago))]
    (relay/send relay ["REQ" id {"kinds" [3] "since" since}])))

(defn request-metadata [relay id since]
  (relay/send relay ["REQ" id {"kinds" [0] "since" since}]))

(defn subscribe-all
  ([relay id]
   (let [now (int (quot (System/currentTimeMillis) 1000))]
     (subscribe-all relay id (- now 86400) now)))
  ([relay id since now]
   (relay/send relay ["REQ" id {"since" since "until" now}])
   (relay/send relay ["REQ" (str id "-all") {"since" now}])))

(defn subscribe-trusted [relay id since now]
  (let [trustee-ids (contact-list/get-trustees)
        trustees (map util/hexify trustee-ids)
        short-trustees (map #(subs % 0 10) trustees)]
    (relay/send relay ["REQ" id {"since" since "until" now "authors" short-trustees}])
    (relay/send relay ["REQ" (str id "-trusted") {"since" now "authors" short-trustees}])))

(defn subscribe-web-of-trust [relay id since now]
  (let [trustee-ids (contact-list/get-web-of-trust)
        trustees (map util/hexify trustee-ids)
        short-trustees (map #(subs % 0 10) trustees)]
    (relay/send relay ["REQ" id {"since" since "until" now "authors" short-trustees}])
    (relay/send relay ["REQ" (str id "-trust-web") {"since" now "authors" short-trustees}])))

(defn unsubscribe [relay id]
  (relay/send relay ["CLOSE" id]))

(defn close-relay [relay]
  (relay/close relay)
  (swap! relays assoc-in [(::ws-relay/url relay) :connection] nil))

(defn close-connection [relay id]
  (unsubscribe relay id)
  (close-relay relay)
  )

(defn handle-relay-message [relay message]
  (let [url (::ws-relay/url relay)]
    (update-mem :websocket-backlog inc)
    (update-mem :incoming-events inc)
    (update-mem [:events-by-relay url] #(if (nil? %) 1 (inc %)))
    (send-off events/event-agent handlers/handle-event message url)))

(defn subscribe-to-relay [url id since now]
  (let [relay (get-in @relays [url :connection])
        read-type (get-in @relays [url :read])]
    (when (and (or (not= :false read-type)
                   (not= :no-read read-type))
               (some? relay))
      (condp = read-type
        true (subscribe-all relay id since now)
        :read-all (subscribe-all relay id since now)
        :read-trusted (subscribe-trusted relay id since now)
        :read-web-of-trust (subscribe-web-of-trust relay id since now)
        nil)
      (swap! relays assoc-in [url :subscribed] true))))

(defn subscribe-to-relays [id subscription-time now]
  (let [date (- subscription-time 100)]
    (prn 'subscription-date date (format-time date))
    (doseq [url (keys @relays)]
      (subscribe-to-relay url id date now))))

(defn connect-to-relay [relay]
  (let [url (::ws-relay/url relay)
        relay-config (get @relays url)
        read-type (:read relay-config)
        readable? (or (= read-type true)
                      (= read-type :read-all)
                      (= read-type :read-trusted)
                      (= read-type :read-web-of-trust))
        writeable? (:write relay-config)
        should-connect? (or readable? writeable?)
        open-relay (if should-connect?
                     (relay/open relay)
                     nil)]
    (when (some? open-relay)
      (swap! relays assoc-in [url :connection] open-relay))))

(defn increment-relay-retry
  "used in a swap!, increments retries counter unless the last retry was over an hour ago
  and then resets to 1"
  [relays url]
  (let [relay (get relays url)
        retries (get relay :retries 0)
        last-retry-time (get relay :retry-time 0)
        now (util/get-now-ms)
        time-since-last-retry (- now last-retry-time)
        retries (if (> time-since-last-retry 3600000) 1 (inc retries))
        relays (assoc-in relays [url :retries] retries)
        relays (assoc-in relays [url :retry-time] now)]
    relays))

(defn handle-close [relay]
  (let [url (::ws-relay/url relay)
        now (quot (System/currentTimeMillis) 1000)
        minutes-10 600
        date (- now minutes-10)
        retrying? (get-in @relays [url :retrying])
        active? (is-active-url? url)]
    (when (and (not retrying?) active?)
      (prn 'relay-closed url)
      (swap! relays assoc-in [url :retrying] true)
      (swap! relays assoc-in [url :connection] nil)
      (swap! relays increment-relay-retry url)
      (future
        (let [retries (get-in @relays [url :retries])
              seconds-to-wait (min 300 (* retries 30))]
          (prn 'retries retries url)
          (prn 'waiting seconds-to-wait 'seconds url)
          (Thread/sleep (* 1000 seconds-to-wait))
          (swap! relays assoc-in [url :retrying] false)
          (prn 'reconnecting-to url)
          (connect-to-relay relay)
          (subscribe-to-relay url (str config/subscription-id-base "-reconnect") date now))))))

(defn make-relay [url]
  (ws-relay/make url {:recv handle-relay-message
                      :close handle-close}))
(defn connect-to-relays []
  (doseq [url (keys @relays)]
    (let [relay (make-relay url)]
      (connect-to-relay relay)))
  (prn 'relay-connection-attempts-complete))

(defn request-contact-lists-from-relays [id]
  (prn 'requesting-contact-lists)
  (doseq [url (keys @relays)]
    (let [relay (get-in @relays [url :connection])
          read-type (get-in @relays [url :read])]
      (when (= :read-all read-type)
        (request-contact-lists relay id)))))

(defn request-metadata-from-relays [id since]
  (prn 'requesting-metadata)
  (doseq [url (keys @relays)]
    (let [relay (get-in @relays [url :connection])
          read? (get-in @relays [url :read])]
      (when (and read? (some? relay))
        (request-metadata relay id since)))))

(defn unsubscribe-from-relays [id]
  (doseq [url (keys @relays)]
    (let [relay (get-in @relays [url :connection])]
      (when (some? relay)
        (prn 'closing url)
        (close-connection relay id)))))


