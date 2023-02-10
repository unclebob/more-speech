(ns more-speech.websocket-relay
  (:require [more-speech
             [relay :as relay]]
            [clojure.data.json :as json])
  (:import (java.net.http HttpClient WebSocket$Listener WebSocket)
           (java.net URI)
           (java.util Timer TimerTask)
           (java.nio ByteBuffer)))

(defn to-json [o]
  (json/write-str o :escape-slash false :escape-unicode false))

;callbacks :recv :close
(defn make [url callbacks]
  {::relay/type ::websocket
   ::url url
   ::callbacks callbacks
   ::socket nil})

(defn handle-text [{:keys [buffer relay]} data last]
  (let [{::keys [url callbacks]} relay]
    (.append buffer (.toString data))
    (when last
      (try
        (let [envelope (json/read-str (.toString buffer))]
          ((:recv callbacks) relay envelope))
        (catch Exception e
          (prn 'onText url (.getMessage e))
          (prn (.toString buffer))))
      (.delete buffer 0 (.length buffer)))))

(defrecord listener [buffer relay]
  WebSocket$Listener
  (onOpen [_this _webSocket]
    (prn 'open (::url relay)))
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
  (onClose [_this _webSocket _statusCode _reason]
    ((:close (::callbacks relay)) relay))
  (onError [_this _webSocket error]
    (prn 'websocket-listener-error (::url relay) (:cause error))
    ((:close (::callbacks relay)) relay))
  )

(defn send-ping [relay]
  (let [{::keys [socket]} relay]
    (.sendPing socket (ByteBuffer/allocate 4))))

(defn start-timer [relay]
  (let [timer (Timer. (format "Ping timer for %s" (::url relay)))
        ping-task (proxy [TimerTask] []
                    (run [] (send-ping relay)))]
    (.schedule timer ping-task (long 30000) (long 30000))
    timer))

(defmethod relay/open ::websocket [relay]
  (let [{::keys [url]} relay]
    (try
      (let [client (HttpClient/newHttpClient)
            cl (.newWebSocketBuilder client)
            cws (.buildAsync cl (URI/create url) (->listener (StringBuffer.) relay))
            wsf (future (.get cws))
            ws (deref wsf 5000 :time-out)]
        (if (= ws :time-out)
          (do
            (prn 'connection-time-out url)
            relay)
          (let [open-relay (assoc relay ::socket ws)]
            (assoc open-relay :timer (start-timer open-relay)))))
      (catch Exception e
        (prn 'connect-to-relay-failed url (:reason e))
        (future ((:close (::callbacks relay)) relay))
        relay))))

(defmethod relay/close ::websocket [relay]
  (let [{::keys [socket timer]} relay]
    (when (and socket (not (.isOutputClosed socket)))
      (try
        (.get (.sendClose socket WebSocket/NORMAL_CLOSURE "done"))
        (catch Exception e
          (prn 'on-send-close-error (:reason e)))))
    (when timer (.cancel timer))
    (assoc relay ::socket nil)))

(defmethod relay/send ::websocket [relay message]
  (let [{::keys [socket url]} relay]
    (when (and socket (not (.isOutputClosed socket)))
      (try
        (let [json (to-json message)]
          (println "sending to:" url " " json)
          (.sendText socket json true)
          (.request socket 1))
        (catch Exception e
          (prn 'send-to (.getMessage e)))))))

