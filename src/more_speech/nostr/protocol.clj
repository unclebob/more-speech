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
             "wss://expensive-relay.fiatjaf.com"
             "wss://wlvs.space"
             "wss://nostr.rocks"
             "wss://nostr-relay.herokuapp.com"
             "wss://freedom-relay.herokuapp.com/ws"         ;*
             "wss://nodestr-relay.dolu.dev/ws"
             "wss://nostrrr.bublina.eu.org"
             "wss://nostr-relay.freeberty.ne"
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

(defrecord listener [buffer event-agent]
  WebSocket$Listener
  (onOpen [_this _webSocket]
    (prn 'open))
  (onText [_this webSocket data last]
    (.append buffer (.toString data))
    (when last
      (send event-agent events/process-event (json/read-str (.toString buffer)))
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
    (prn 'close statusCode reason)
    )
  (onError [_this _webSocket error]
    (prn 'websocket-listener-error error)
    )
  )

(defn connect-to-relay ^WebSocket [url event-agent]
  (let [client (HttpClient/newHttpClient)
        cl (.newWebSocketBuilder client)
        cws (.buildAsync cl (URI/create url) (->listener (StringBuffer.) event-agent))
        ws (.get cws)
        ]
    ws)
  )

(defn get-events [event-agent]
  (let [conn (connect-to-relay (get relays 0) event-agent)
        id "more-speech"
        date (make-date "04/20/2022")
        send-chan (:send-chan @event-agent)
        ]
    (prn date (format-time date))
    (unsubscribe conn id)
    (subscribe conn id date)
    (loop [msg (async/<!! send-chan)]
      (condp = (first msg)
        :closed nil
        :event
        (do
          (send-to conn (second msg))
          (recur (async/<!! send-chan)))))
    (unsubscribe conn id)
    (.get (.sendClose conn WebSocket/NORMAL_CLOSURE "done"))
    (Thread/sleep 100))
  (prn 'done)
  )

(defn close-connection [state]
  (prn 'close-connection)
  (async/>!! (:send-chan state) [:closed])
  )
