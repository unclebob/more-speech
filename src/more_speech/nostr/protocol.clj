(ns more-speech.nostr.protocol
  (:require [clojure.data.json :as json]
            [clojure.core.async :as async]
            [more-speech.nostr.events :as events]
            [more-speech.nostr.relays :refer [relays]]
            [more-speech.nostr.util :as util]
            [more-speech.user-configuration :as user-configuration]
            [clojure.stacktrace :as st])
  (:import (java.util Date)
           (java.text SimpleDateFormat)
           (java.net.http WebSocket HttpClient WebSocket$Listener)
           (java.net URI)))

(defn send-to [^WebSocket conn msg]
  (try
    (let [msg (events/to-json msg)]
      (println "sending:" msg)
      (.sendText conn msg true))
    (catch Exception e
      (prn 'send-to (.getMessage e)))))

(defn format-time [time]
  (let [time (* time 1000)
        date (Date. (long time))]
    (.format (SimpleDateFormat. "MM/dd/yyyy kk:mm:ss z") date))
  )

(defn request-contact-lists [^WebSocket conn id]
  (send-to conn ["REQ" id {"kinds" [3] "since" 0}])
  (.request conn 1))

(defn request-metadata [^WebSocket conn id]
  (send-to conn ["REQ" id {"kinds" [0] "since" 0}])
  (.request conn 1))

(defn subscribe
  ([conn id]
   (subscribe conn id (int (- (quot (System/currentTimeMillis) 1000) 86400))))
  ([^WebSocket conn id since]
   (send-to conn ["REQ" id {"since" since}])
   (.request conn 1)))

(defn unsubscribe [^WebSocket conn id]
  (send-to conn ["CLOSE" id]))

(declare record-and-display-event)
(def event-agent (agent nil))

(defn handle-text [{:keys [buffer event-context url]} data last]
  (.append buffer (.toString data))
  (when last
    (try
      (let [envelope (json/read-str (.toString buffer))]
        (send event-agent record-and-display-event event-context envelope url))
      (catch Exception e
        (prn 'onText url (.getMessage e))
        (prn (.toString buffer))))
    (.delete buffer 0 (.length buffer))))

(defn compute-id [event]
  (util/bytes->num
    (events/make-id
      {:pubkey (get event "pubkey")
       :created_at (get event "created_at")
       :kind (get event "kind")
       :tags (get event "tags")
       :content (get event "content")})))

(defn record-and-display-event [_agent event-context envelope url]
  (try
    (let [[_name _subscription-id inner-event :as _decoded-msg] envelope
          event (events/translate-event inner-event)
          id (:id event)
          computed-id (compute-id inner-event)
          ui-handler (:event-handler @event-context)
          dup? (contains? (:text-event-map @event-context) id)]
      (if (= id computed-id)
        (do
          (swap! event-context events/process-event event url)
          (when (and (not dup?) (= (:kind event) 1))
            (events/handle-text-event ui-handler event)
            ))
        (prn 'id-mismatch url 'computed-id (util/num32->hex-string computed-id) envelope)
        ))
    (catch Exception e
      (do (prn `record-and-display-event url (.getMessage e))
          (st/print-stack-trace e))
      )))

(defrecord listener [buffer event-context url]
  WebSocket$Listener
  (onOpen [_this _webSocket]
    (prn 'open url))
  (onText [this webSocket data last]
    (handle-text this data last)
    (.request webSocket 1))
  (onBinary [_this _webSocket _data _last]
    (prn 'binary))
  (onPing [_this webSocket message]
    (.sendPong webSocket message)
    (.request webSocket 1))
  (onPong [_this webSocket _message]
    (.request webSocket 1))
  (onClose [_this _webSocket statusCode reason]
    (prn 'close url statusCode reason))
  (onError [_this _webSocket error]
    (prn 'websocket-listener-error url error))
  )

(defn connect-to-relay ^WebSocket [url event-context]
  (try
    (let [client (HttpClient/newHttpClient)
          cl (.newWebSocketBuilder client)
          cws (.buildAsync cl (URI/create url) (->listener (StringBuffer.) event-context url))
          wsf (future (.get cws))
          ws (deref wsf 1000 :time-out)]
      (if (= ws :time-out)
        (do
          (prn 'connection-time-out url)
          nil)
        ws))
    (catch Exception e
      (prn 'connect-to-relay-failed url (:reason e))
      nil))
  )

(defn close-connection [conn id]
  (unsubscribe conn id)
  (try
    (.get (.sendClose conn WebSocket/NORMAL_CLOSURE "done"))
    (catch Exception e
      (prn 'on-send-close-error (:reason e)))))

(defn connect-to-relays [event-context]
  (doseq [url (keys @relays)]
    (let [relay (get @relays url)
          should-connect? (or (:read relay) (:write relay))
          connection (if should-connect?
                       (connect-to-relay url event-context)
                       nil)]
      (when (some? connection)
        (swap! relays assoc-in [url :connection] connection))))
  (prn 'relay-connection-attempts-complete))


(defn request-contact-lists-from-relays [id]
  (prn 'requesting-contact-lists)
  (doseq [url (keys @relays)]
    (let [conn (get-in @relays [url :connection])
          read? (get-in @relays [url :read])]
      (when (and read? (some? conn))
        (unsubscribe conn id)
        (request-contact-lists conn id)))))

(defn request-metadata-from-relays [id]
  (prn 'requesting-metadata)
  (doseq [url (keys @relays)]
    (let [conn (get-in @relays [url :connection])
          read? (get-in @relays [url :read])]
      (when (and read? (some? conn))
        (unsubscribe conn id)
        (request-metadata conn id)))))

(defn subscribe-to-relays [id subscription-time]
  (let [date (- subscription-time 100)]
    (prn 'subscription-date date (format-time date))
    (doseq [url (keys @relays)]
      (let [conn (get-in @relays [url :connection])
            read? (get-in @relays [url :read])]
        (when (and read? (some? conn))
          (unsubscribe conn id)
          (subscribe conn id date)
          (swap! relays assoc-in [url :subscribed] true))))))

(defn process-send-channel [event-context]
  (let [send-chan (:send-chan @event-context)
        urls (keys @relays)
        send-urls (filter #(:write (get @relays %)) urls)
        send-connections (map #(get-in @relays [% :connection]) send-urls)
        send-connections (filter some? send-connections)]
    (loop [[type msg] (async/<!! send-chan)]
      (condp = type
        :closed :quit
        :relaunch :relaunch
        :event
        (do
          (doseq [send-connection send-connections] (send-to send-connection msg))
          (recur (async/<!! send-chan)))))))

(defn unsubscribe-from-relays [id]
  (doseq [url (keys @relays)]
    (let [conn (get-in @relays [url :connection])]
      (when (some? conn)
        (prn 'closing url)
        (close-connection conn id)))))

(defn get-events [event-context subscription-time]
  (let [subscription-id "more-speech"
        metadata-request-id "more-speech-metadata"
        contact-lists-request-id "more-speech-contact-lists"
        event-handler (:event-handler @event-context)
        now-in-seconds (quot (System/currentTimeMillis) 1000)]
    (connect-to-relays event-context)
    (request-contact-lists-from-relays contact-lists-request-id)
    (when (user-configuration/should-import-metadata? now-in-seconds)
      (request-metadata-from-relays metadata-request-id)
      (user-configuration/set-last-time-metadata-imported now-in-seconds))
    (subscribe-to-relays subscription-id subscription-time)
    (events/update-relay-panel event-handler)
    (if (user-configuration/should-export-profile? now-in-seconds)
      (do
        (user-configuration/set-last-time-profile-exported now-in-seconds)
        (future (events/compose-and-send-metadata-event @event-context)))
      (println "Not time to export profile yet."))
    (let [exit-condition (process-send-channel event-context)]
      (unsubscribe-from-relays subscription-id)
      (Thread/sleep 100)
      (prn 'done)
      exit-condition))
  )