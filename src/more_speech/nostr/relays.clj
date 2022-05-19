(ns more-speech.nostr.relays
  (:require [clojure.spec.alpha :as s]))

(defn- connection? [c]
  (= (type c) 'java.net.http.WebSocket))
(s/def ::read boolean?)
(s/def ::write boolean?)
(s/def ::subscribed boolean?)
(s/def ::connection (s/nilable connection?))
(s/def ::relay (s/keys :req-un [::read ::write ::subscribed ::connection]))
(s/def ::relays (s/map-of string? ::relay))


;(def old-relays ["wss://nostr-pub.wellorder.net"
;                 ;"wss://expensive-relay.fiatjaf.com"
;                 "wss://wlvs.space"
;                 "wss://nostr.rocks"
;                 ;"wss://nostr-relay.herokuapp.com"
;                 "wss://freedom-relay.herokuapp.com/ws"
;                 ;"wss://nodestr-relay.dolu.dev/ws"
;                 ;"wss://nostrrr.bublina.eu.org"
;                 ;"wss://nostr-relay.freeberty.ne"
;                 "ws://nostr.rocks:7448"
;                 "ws://nostr-pub.wellorder.net:7000"
;                 ])


(def relays (atom nil))

(defn set-relay-defaults [relays]
  (loop [urls (keys relays)
         relays relays]
    (if (empty? urls)
      relays
      (let [url (first urls)
            relay (get relays url)
            relay (assoc relay :connection nil :subscribed false)]
        (recur (rest urls) (assoc relays url relay))))))

(defn load-relays [relay-text]
  (let [loaded-relays (if (empty? relay-text)
                        {}
                        (read-string relay-text))
        loaded-relays (set-relay-defaults loaded-relays)
        ]
    (reset! relays loaded-relays)))

(defn load-relays-from-file [file-name]
  (load-relays (slurp file-name)))