(ns more-speech.nostr.relays)


(def old-relays ["wss://nostr-pub.wellorder.net"
                 ;"wss://expensive-relay.fiatjaf.com"
                 "wss://wlvs.space"
                 "wss://nostr.rocks"
                 ;"wss://nostr-relay.herokuapp.com"
                 "wss://freedom-relay.herokuapp.com/ws"
                 ;"wss://nodestr-relay.dolu.dev/ws"
                 ;"wss://nostrrr.bublina.eu.org"
                 ;"wss://nostr-relay.freeberty.ne"
                 "ws://nostr.rocks:7448"
                 "ws://nostr-pub.wellorder.net:7000"
                 ])


(def relays (atom {"wss://nostr-pub.wellorder.net"
                   {:read true
                    :write true
                    :connection nil
                    :subscribed false}
                   "wss://wlvs.space"
                   {:read true
                    :write true
                    :connection nil
                    :subscribed false}
                   "wss://nostr.rocks"
                   {:read true
                    :write true
                    :connection nil
                    :subscribed false}
                   "wss://freedom-relay.herokuapp.com/ws"
                   {:read true
                    :write true
                    :connection nil
                    :subscribed false}
                   "ws://nostr.rocks:7448"
                   {:read true
                    :write true
                    :connection nil
                    :subscribed false}
                   "ws://nostr-pub.wellorder.net:7000"
                   {:read true
                    :write true
                    :connection nil
                    :subscribed false}
                   }))

(defn set-connections [relays conn-status]
  (map #(assoc %1 :connected %2) relays conn-status))