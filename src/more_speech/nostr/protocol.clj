(ns more-speech.nostr.protocol
  (:require [clojure.data.json :as json]
            [clojure.core.async :as async]
            [more-speech.nostr.events :as events])
  (:import (java.util Date)
           (java.text SimpleDateFormat)
           (java.net.http WebSocket HttpClient WebSocket$Listener)
           (java.net URI)
           ))

(def relays ["wss://nostr-pub.wellorder.net"                ;*
             ;"wss://expensive-relay.fiatjaf.com"
             "wss://wlvs.space"
             "wss://nostr.rocks"
             ;"wss://nostr-relay.herokuapp.com"
             "wss://freedom-relay.herokuapp.com/ws"         ;*
             ;"wss://nodestr-relay.dolu.dev/ws"
             ;"wss://nostrrr.bublina.eu.org"
             ;"wss://nostr-relay.freeberty.ne"
             "ws://nostr.rocks:7448"                        ;*
             "ws://nostr-pub.wellorder.net:7000"
             ])

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

(defrecord listener [buffer event-agent url]
  WebSocket$Listener
  (onOpen [_this _webSocket]
    (prn 'open url))
  (onText [_this webSocket data last]
    (.append buffer (.toString data))
    (when last
      (let [event (json/read-str (.toString buffer))]
        (send event-agent events/process-event event url))
      (.delete buffer 0 (.length buffer)))
    (.request webSocket 1)
    )
  (onBinary [_this _webSocket _data _last]
    (prn 'binary)
    )
  (onPing [_this webSocket message]
    (prn 'ping)
    (.sendPong webSocket message)
    (.request webSocket 1)
    (prn 'sent-pong)
    )
  (onPong [_this webSocket _message]
    (prn 'pong)
    (.request webSocket 1)
    )
  (onClose [_this _webSocket statusCode reason]
    (prn 'close url statusCode reason)
    )
  (onError [_this _webSocket error]
    (prn 'websocket-listener-error url error)
    )
  )

(defn connect-to-relay ^WebSocket [url event-agent]
  (try
    (let [client (HttpClient/newHttpClient)
          cl (.newWebSocketBuilder client)
          cws (.buildAsync cl (URI/create url) (->listener (StringBuffer.) event-agent url))
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

(defn get-events [event-agent]
  (let [connections (map #(connect-to-relay % event-agent) relays)
        connections (filter some? connections)
        id "more-speech"
        date (make-date "04/20/2022")
        send-chan (:send-chan @event-agent)
        ]
    (prn date (format-time date))
    (doseq [conn connections]
      (unsubscribe conn id)
      (subscribe conn id date))
    (loop [msg (async/<!! send-chan)]
      (condp = (first msg)
        :closed nil
        :event
        (do
          (send-to (first connections) (second msg))
          (recur (async/<!! send-chan)))))
    (doseq [conn connections]
      (close-connection conn id)
      )
    (Thread/sleep 100))
  (prn 'done)
  )