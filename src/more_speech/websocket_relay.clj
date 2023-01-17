(ns more-speech.websocket-relay
  (:require [more-speech
             [relay :as relay]]
            [clojure.data.json :as json])
  (:import (java.net.http HttpClient WebSocket$Listener WebSocket)
           (java.net URI)))

(defn to-json [o]
  (json/write-str o :escape-slash false :escape-unicode false))

(defn make [url recv-f]
  {::relay/type ::websocket
   ::url url
   ::recv-f recv-f
   ::socket nil
   ::open? false})

(defn handle-text [{:keys [buffer relay]} data last]
  (let [{::keys [url recv-f]} relay]
    (.append buffer (.toString data))
    (when last
      (try
        (let [envelope (json/read-str (.toString buffer))]
          (recv-f envelope))
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
  (onClose [_this _webSocket statusCode reason]
    (prn 'close (::url relay) statusCode reason))
  (onError [_this _webSocket error]
    (prn 'websocket-listener-error (::url relay) error))
  )

(defmethod relay/open ::websocket [relay]
  (let [{::keys [url]} relay]
    (try
      (let [client (HttpClient/newHttpClient)
            cl (.newWebSocketBuilder client)
            cws (.buildAsync cl (URI/create url) (->listener (StringBuffer.) relay))
            wsf (future (.get cws))
            ws (deref wsf 1000 :time-out)]
        (if (= ws :time-out)
          (do
            (prn 'connection-time-out url)
            (assoc relay ::open? false))
          (assoc relay ::open? true ::socket ws)))
      (catch Exception e
        (prn 'connect-to-relay-failed url (:reason e))
        (assoc relay ::open? false)))))

(defmethod relay/close ::websocket [relay]
  (let [{::keys [socket]} relay]
    (try
      (.get (.sendClose socket WebSocket/NORMAL_CLOSURE "done"))
      (catch Exception e
        (prn 'on-send-close-error (:reason e))))
    (assoc relay ::open? false ::socket nil)))

(defmethod relay/send ::websocket [relay message]
  (let [{::keys [socket url]} relay]
    (try
      (let [json (to-json message)]
        (println "sending to:" url " " json)
        (.sendText socket json true)
        (.request socket 1))
      (catch Exception e
        (prn 'send-to (.getMessage e))))))

