(ns more-speech.nostr.zaps-spec
  (:require
    [clojure.core.async :as async]
    [more-speech.bech32 :as bech32]
    [more-speech.config :as config]
    [more-speech.db.gateway :as gateway]
    [more-speech.mem :refer :all]
    [more-speech.nostr.elliptic-signature :as es]
    [more-speech.nostr.event-composers :as composers]
    [more-speech.nostr.util :as util]
    [more-speech.nostr.zaps :as zaps]
    [more-speech.relay :as relay]
    [more-speech.spec-util :refer :all]
    [more-speech.util.fortune-messages :as fortune]
    [more-speech.websocket-relay :as ws-relay]
    [speclj.core :refer :all])
  (:import (ecdhJava SECP256K1)))

(declare db)
(describe "zaps"
  (with-stubs)
  (setup-db-mem)

  (context "zap address"
    (context "zap address is in tag"
      (it "determines lud16 zap address from zap tag"
        (let [event {:tags [[:zap "me@you.xxx" "lud16"]]}]
          (should= "https://you.xxx/.well-known/lnurlp/me"
                   (zaps/get-lnurl event))))

      (it "passes mistyped address"
        (let [event {:tags [[:zap "zap-address" "xxx"]]}]
          (should= "zap-address" (zaps/get-lnurl event))))

      (it "passes untyped address"
        (let [event {:tags [[:zap "zap-address"]]}]
          (should= "zap-address" (zaps/get-lnurl event))))
      )

    (context "No zap tag"
      (it "rejects if no profile for author"
        (let [event {:pubkey 1}]
          (should-throw Exception "no zap tag or profile"
                        (zaps/get-lnurl event))))

      (it "rejects if profile has no lud16"
        (let [event {:pubkey 1}]
          (gateway/add-profile @db 1 {:name "somebody"})
          (should-throw Exception "no zap tag or profile"
                        (zaps/get-lnurl event))))

      (it "gets lud16 zap addr from profile"
        (let [event {:pubkey 1}]
          (gateway/add-profile @db 1 {:name "somebody"
                                      :lud16 "me@you.xxx"})
          (should= "https://you.xxx/.well-known/lnurlp/me"
                   (zaps/get-lnurl event))))

      (it "gets lud06 zap addr from profile"
        (let [event {:pubkey 1}
              lnurl (bech32/encode-str "lnurl" "the-lnurl")]
          (gateway/add-profile @db 1 {:name "somebody"
                                      :lud06 lnurl})
          (should= "the-lnurl" (zaps/get-lnurl event))))
      )

    (context "lud16 parsing"
      (it "rejects bad lud16 formats"
        (should-throw Exception "bad lud16 format xxxx"
                      (zaps/parse-lud16 "xxxx"))
        (should-throw Exception "bad lud16 format no-colons:j@z.com"
                      (zaps/parse-lud16 "no-colons:j@z.com"))
        (should-throw Exception "bad lud16 format name@need-a-dot"
                      (zaps/parse-lud16 "name@need-a-dot")))

      (it "parses a good lud16 format"
        (should= ["name" "domain.xxx"]
                 (zaps/parse-lud16 "name@domain.xxx"))
        (should= ["name.of-me_32" "domain.32-x_t.c-t"]
                 (zaps/parse-lud16 "name.of-me_32@domain.32-x_t.c-t")))
      )

    (context "lnurl"
      (it "makes lnurl from lud16"
        (should= "https://domain.xxx/.well-known/lnurlp/name"
                 (zaps/lud16->lnurl "name@domain.xxx")))
      )

    (context "zap request"
      (it "makes a zap request if all is valid"
        (with-redefs [util/get-now (stub :get-now {:return 11111})]
          (let [wallet-response {"callback" "callback"
                                 "maxSendable" 100
                                 "minSendable" 1
                                 "metadata" "metadata"
                                 "tag" "payRequest"
                                 "commentAllowed" 20
                                 "allowsNostr" true
                                 "nostrPubkey" "deadbeef"}
                recipient-id 99
                event-id 1
                event {:pubkey recipient-id :id event-id}
                amount 100
                comment "comment"
                lnurl "lnurl"
                b32-lnurl (bech32/encode-str "lnurl" lnurl)
                my-privkey 0xb0b
                my-pubkey (util/bytes->num (es/get-pub-key (util/num->bytes 32 my-privkey)))
                _ (set-mem :pubkey my-pubkey)
                _ (set-mem [:keys :private-key] (util/hexify my-privkey))
                _ (set-mem [:keys :public-key] (util/hexify my-pubkey))
                _ (set-mem :relays {"relay-r1" {:read :read-all}
                                    "relay-nr" {:read :read-none}
                                    "relay-r2" {:read :read-all}})
                body (zaps/make-zap-request
                       wallet-response event amount comment lnurl)
                {:keys [kind content tags pubkey created_at]} body
                tags (set tags)]

            (should= 9734 kind)
            (should= "comment" content)
            (should= my-pubkey (util/unhexify pubkey))
            (should= (util/get-now) created_at)
            (should (contains? tags ["relays" "relay-r1" "relay-r2"]))
            (should (contains? tags ["amount" "100"]))
            (should (contains? tags ["lnurl" b32-lnurl]))
            (should (contains? tags ["p" (util/hexify recipient-id)]))
            (should (contains? tags ["e" (util/hexify event-id)])))))

      (it "rejects if nostr is not allowed"
        (let [wallet-response {"callback" "callback"
                               "maxSendable" 100
                               "minSendable" 1
                               "metadata" "metadata"
                               "tag" "payRequest"
                               "commentAllowed" 20
                               "allowsNostr" false
                               "nostrPubkey" "deadbeef"}
              amount 100
              event {}
              comment "12345678901234567890"
              lnurl "lnurl"]
          (should-throw Exception "Recipient does not accept Nostr zaps."
                        (zaps/make-zap-request wallet-response
                                               event amount comment lnurl))))

      (it "rejects if amount too small"
        (let [wallet-response {"callback" "callback"
                               "maxSendable" 1000000
                               "minSendable" 1000
                               "metadata" "metadata"
                               "tag" "payRequest"
                               "commentAllowed" 20
                               "allowsNostr" true
                               "nostrPubkey" "deadbeef"}
              amount 100
              event {}
              comment "12345678901234567890"
              lnurl "lnurl"]
          (should-throw Exception "Amount 0 is below minimum of 1"
                        (zaps/make-zap-request wallet-response
                                               event amount comment lnurl))))

      (it "rejects if amount too large"
        (let [wallet-response {"callback" "callback"
                               "maxSendable" 1000000
                               "minSendable" 1000
                               "metadata" "metadata"
                               "tag" "payRequest"
                               "commentAllowed" 20
                               "allowsNostr" true
                               "nostrPubkey" "deadbeef"}
              event {}
              comment "12345678901234567890"
              lnurl "lnurl"
              amount 2000000]
          (should-throw Exception "Amount 2000 is larger than maximum of 1000"
                        (zaps/make-zap-request wallet-response
                                               event amount comment lnurl))))

      (it "rejects if comment too long"
        (let [wallet-response {"callback" "callback"
                               "maxSendable" 1000000
                               "minSendable" 1000
                               "metadata" "metadata"
                               "tag" "payRequest"
                               "commentAllowed" 20
                               "allowsNostr" true
                               "nostrPubkey" "deadbeef"}
              comment "123456789012345678901"
              amount 1000
              event {}
              lnurl "lnurl"]
          (should-throw Exception "This wallet restricts comments to 20 characters"
                        (zaps/make-zap-request wallet-response
                                               event amount comment lnurl))))
      )
    )

  (context "auto-thanks"
    (it "sends thanks for a zap when auto-thanks is :on"
      (with-redefs [composers/compose-and-send-text-event (stub :send)
                    config/auto-thanks :on
                    config/auto-thanks-fortune :off]
        (let [zapper-id (rand-int 1000000)]
          (gateway/add-profile @db zapper-id {:name "zapper"})
          (zaps/auto-thanks zapper-id)
          (should-have-invoked :send {:with [nil "Auto Thanks" "@zapper Thank you!\n"]}))))

    (it "dms thanks for a zap when auto-thanks is :dm"
      (with-redefs [composers/compose-and-send-text-event (stub :send)
                    config/auto-thanks :dm
                    config/auto-thanks-fortune :off]
        (let [zapper-id (rand-int 1000000)]
          (gateway/add-profile @db zapper-id {:name "zapper"})
          (zaps/auto-thanks zapper-id)
          (should-have-invoked :send {:with [nil "Auto Thanks" "D @zapper Thank you!\n"]}))))

    (it "sends thanks for a zap with a fortune"
      (with-redefs [composers/compose-and-send-text-event (stub :send)
                    fortune/get-message (stub :get-message {:return "hi"})
                    config/auto-thanks :on
                    config/auto-thanks-fortune :normal]
        (let [zapper-id (rand-int 1000000)]
          (gateway/add-profile @db zapper-id {:name "zapper"})
          (zaps/auto-thanks zapper-id)
          (should-have-invoked :send {:with [nil "Auto Thanks" "@zapper Thank you!\nhi"]}))))
    )

  (context "wallet-connect"
    (it "executes wallet-connect payment"
      (let [event-index (atom -1)
            events [{:content "pay_invoice"} {:kind 23195 :content "{\"result_type\": \"pay_invoice\"}"}]]
        (set-mem [:keys :wallet-connect] "nostrwalletconnect://beef?relay=wc-relay-url&secret=dead")
        (with-redefs [ws-relay/make (stub :relay-make {:return "some-relay"})
                      relay/open (stub :relay-open {:return "open-relay"})
                      relay/send (stub :relay-send)
                      relay/close (stub :relay-close)
                      zaps/get-wc-request-event (stub :request-event {:return "request-event"})
                      async/<!! (stub :read-chan {:invoke (fn [_x] (get events (swap! event-index inc)))})]
          (let [event-chan :some-channel]
            (zaps/zap-by-wallet-connect "event-to-zap" event-chan)
            (should-have-invoked :relay-make {:with ["wc-relay-url" :*]})
            (should-have-invoked :relay-open {:with ["some-relay"]})
            (should-have-invoked :relay-send {:with ["open-relay" ["REQ" "ms-info" {"kinds" [13194], "authors" ["beef"]}]]})
            (should-have-invoked :request-event {:with ["event-to-zap" :*]})
            (should-have-invoked :relay-send {:with ["open-relay" "request-event"]})
            (should-have-invoked :relay-close {:with ["open-relay"]})))))

    (it "generates wc request"
      (should= "{\"method\":\"pay_invoice\",\"params\":{\"invoice\":\"invoice\"}}"
               (zaps/make-wc-json-request "invoice")))

    (it "composes a wc request event"
      (with-redefs [config/proof-of-work-default 0]
        (let [sender-private-key-bytes (util/make-private-key)
              sender-private-key (util/bytes->num sender-private-key-bytes)
              sender-private-key-hex (util/hexify sender-private-key)
              sender-public-key-bytes (es/get-pub-key sender-private-key-bytes)
              sender-public-key (util/bytes->num sender-public-key-bytes)
              recipient-private-key-bytes (util/make-private-key)
              recipient-private-key (util/bytes->num recipient-private-key-bytes)
              recipient-public-key-bytes (es/get-pub-key recipient-private-key-bytes)
              recipient-public-key (util/bytes->num recipient-public-key-bytes)
              recipient-public-key-hex (util/hexify recipient-public-key)
              inbound-shared-secret (SECP256K1/calculateKeyAgreement
                                      recipient-private-key
                                      sender-public-key)
              request "request"
              _ (set-mem :pubkey 1)
              [_ event] (zaps/compose-wc-request-event recipient-public-key-hex sender-private-key-hex request)]
          (should= 23194 (:kind event))
          (should= [[:p recipient-public-key-hex]] (filter #(= :p (first %)) (:tags event)))
          (should= request (SECP256K1/decrypt inbound-shared-secret (:content event)))))))
  )

