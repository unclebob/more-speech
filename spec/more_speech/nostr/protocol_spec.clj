(ns more-speech.nostr.protocol-spec
  (:require [speclj.core :refer :all]
            [more-speech.nostr.protocol :refer :all]
            [more-speech.nostr.elliptic-signature :as ecc]))

(def relays ["wss://nostr-pub.wellorder.net"
             "wss://relayer.fiatjaf.com"
             "wss://nostr.rocks"
             "wss://nostr-relay.herokuapp.com"
             "wss://freedom-relay.herokuapp.com/ws"
             "wss://nodestr-relay.dolu.dev/ws"
             "wss://nostrrr.bublina.eu.org"
             "wss://nostr-relay.freeberty.ne"
             "ws://nostr.rocks:7448"
             "ws://nostr-pub.wellorder.net:7000"
             ])

(describe "aleph-trial"
  (it "builds and signs a text message"
    (let [text (make-text "hello" (ecc/sha-256 (.getBytes "My Private ID.")))
          [message-type {:strs [id pubkey created_at kind tags content sig]}] text]
      (should= "EVENT" message-type)
      (should= 64 (count id))
      (should= 64 (count pubkey))
      (should (number? (eval created_at)))
      (should= 1 kind)
      (should= [] tags)
      (should= "hello" content)
      (should= 128 (count sig))
      (should (ecc/verify (ecc/hex-string->bytes pubkey)
                          (ecc/hex-string->bytes id)
                          (ecc/hex-string->bytes sig)))
      ))
  )

