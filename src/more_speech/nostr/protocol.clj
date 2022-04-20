(ns more-speech.nostr.protocol
  (:require [clojure.data.json :as json]
            [more-speech.nostr.elliptic-signature :as ecc]
            [clojure.core.async :as async]
            [more-speech.nostr.events :as events])
  (:import (java.util Date)
           (java.text SimpleDateFormat)
           (java.net.http WebSocket HttpClient WebSocket$Listener)
           (java.net URI)))

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

(def name-list (atom {}))
(def messages (atom []))

(defn name-of [pubkey]
  (get @name-list pubkey pubkey))

(defn print-names []
  (doseq [entry @name-list]
    (prn entry)))

(defn subscribe
  ([conn id]
   (subscribe conn id (int (- (/ (System/currentTimeMillis) 1000) 86400))))
  ([^WebSocket conn id since]
   (send-to conn ["REQ" id {"since" since}])
   (.request conn 1)))

(defn unsubscribe [^WebSocket conn id]
  (send-to conn ["CLOSE" id]))

(defrecord listener [buffer events]
  WebSocket$Listener
  (onOpen [_this _webSocket]
    (prn 'open))
  (onText [_this webSocket data last]
    (.append buffer (.toString data))
    (when last
      (swap! events conj (json/read-str (.toString buffer)))
      (.delete buffer 0 (.length buffer)))
    (.request webSocket 1)
    )
  (onBinary [_this _webSocket _data _last]
    (prn 'binary)
    )
  (onPing [_this _webSocket _message]
    (prn 'ping)
    )
  (onPong [_this _webSocket _message]
    (prn 'pong)
    )
  (onClose [_this _webSocket statusCode reason]
    (prn 'close statusCode reason)
    )
  (onError [_this _webSocket error]
    (prn 'error)
    )
  )

(defn connect-to-relay ^WebSocket [url events]
  (let [client (HttpClient/newHttpClient)
        cl (.newWebSocketBuilder client)
        cws (.buildAsync cl (URI/create url) (->listener (StringBuffer.) events))
        ws (.get cws)
        ]
    ws)
  )

(def private-key (ecc/sha-256 (.getBytes "I am Bob.")))

(defn get-events [events send-chan]
  (let [conn (connect-to-relay (get relays 0) events)
        id "more-speech"
        date (make-date "04/01/2022")
        ]
    (prn date (format-time date))
    (unsubscribe conn id)
    (subscribe conn id date)
    ;(send-to conn ["EVENT" {:pubkey "2ef93f01cd2493e04235a6b87b10d3c4a74e2a7eb7c3caf168268f6af73314b5", :created_at 1649969311, :kind 1, :tags [], :content "Hello from more-speech.", :id "b05141aecb7d975ae7df861a13082d98ad76e9f46accc046ba221533877b69c6", :sig "e80411eee168aa620b035ce16622eda24d059859562276305a1c8900559b3bc5ae0505754e5d0f352891717eb34f4495771bcf11c80d01cdd19025a51e99979d"}])
    (loop [msg (async/<!! send-chan)]
      (condp = (first msg)
        :closed nil
        :event
        (do
          (send-to conn (second msg))
          (recur (async/<!! send-chan)))))
    (unsubscribe conn id)
    (.get (.sendClose conn WebSocket/NORMAL_CLOSURE "done"))
    (Thread/sleep 1000))
  (prn 'done)
  )

(defn close-connection [state]
  (prn 'close-connection)
  (async/>!! (:send-chan state) [:closed])
  )
