(ns more-speech.nostr.protocol
  (:require [clojure.data.json :as json]
            [clojure.core.async :as async]
            [more-speech.nostr.events :as events]
            [more-speech.nostr.relays :refer [relays]]
            [java-time :as t])
  (:import (java.util Date)
           (java.text SimpleDateFormat)
           (java.net.http WebSocket HttpClient WebSocket$Listener)
           (java.net URI)
           (java.time ZoneOffset)))

(defn send-to [^WebSocket conn msg]
  (let [msg (events/to-json msg)]
    (println "sending:" msg)
    (.sendText conn msg true)))

(defn format-time [time]
  (let [time (* time 1000)
        date (Date. (long time))]
    (.format (SimpleDateFormat. "MM/dd/yyyy kk:mm:ss z") date))
  )

(defn make-date [date-string]
  (let [date (.parse (SimpleDateFormat. "MM/dd/yyyy") date-string)]
    (quot (.getTime date) 1000)))

(defn subscribe
  ([conn id]
   (subscribe conn id (int (- (/ (System/currentTimeMillis) 1000) 86400))))
  ([^WebSocket conn id since]
   (send-to conn ["REQ" id {"since" since}])
   (.request conn 1)))

(defn unsubscribe [^WebSocket conn id]
  (send-to conn ["CLOSE" id]))

(def handle-text-lock (Object.))

(defn handle-text [{:keys [buffer event-context url]} data last]
  (.append buffer (.toString data))
  (when last
    (try
      (let [envelope (json/read-str (.toString buffer))
            [_name _subscription-id inner-event :as _decoded-msg] envelope
            event (events/translate-event inner-event)
            id (:id event)
            ui-handler (:event-handler @event-context)]
        (locking handle-text-lock                           ;prevent duplicates from other relays from sneaking through.
          (let [dup? (contains? (:text-event-map @event-context) id)]
            (swap! event-context events/process-event event url)
            (when (and (not dup?) (= (:kind event) 1))
              ;handle-text-event is asynchronous.
              (events/handle-text-event ui-handler event)))))
      (catch Exception e
        (prn 'onText url (.getMessage e))
        (prn (.toString buffer))))
    (.delete buffer 0 (.length buffer))))

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
    (prn 'ping url)
    (.sendPong webSocket message)
    (.request webSocket 1)
    (prn 'sent-pong))
  (onPong [_this webSocket _message]
    (prn 'pong url)
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
          ws (.get cws)
          ]
      ws)
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
    (let [connection (connect-to-relay url event-context)]
      (swap! relays assoc-in [url :connection] connection))))

(defn subscribe-to-relays [id]
  (let [
        date (-> (t/local-date-time)
                 (t/minus (t/days 10))
                 (t/adjust (t/local-time 0)))
        date (.toEpochSecond date ZoneOffset/UTC)
        ]
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
        :closed nil
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

(defn get-events [event-context]
  (let [id "more-speech"
        event-handler (:event-handler @event-context)]
    (connect-to-relays event-context)
    (subscribe-to-relays id)
    (events/update-relay-panel event-handler)
    (future (events/compose-and-send-metadata-event @event-context))
    (process-send-channel event-context)
    (unsubscribe-from-relays id))
  (Thread/sleep 100)
  (prn 'done)
  )