(ns more-speech.nostr.zaps-spec
  (:require
    [more-speech.bech32 :as bech32]
    [more-speech.config :as config]
    [more-speech.db.gateway :as gateway]
    [more-speech.db.in-memory :as in-memory]
    [more-speech.mem :refer :all]
    [more-speech.nostr.elliptic-signature :as es]
    [more-speech.nostr.util :as util]
    [more-speech.nostr.zaps :as zaps]
    [speclj.core :refer :all]))

(declare db)
(describe "zaps"
  (with-stubs)
  (with db (in-memory/get-db))
  (before-all (config/set-db! :in-memory))
  (before (in-memory/clear-db @db))
  (before (clear-mem))

  (context "zap address"
    (context "zap address is in tag"
      (it "determines lud16 zap address from zap tag"
        (let [event {:tags [[:zap "zap-address" "lud16"]]}]
          (should= "zap-address" (zaps/get-zap-address event))))

      (it "rejects zap address when there are conflicting zap tags"
        (let [event {:tags [[:zap "zap-address" "lud16"]
                            [:zap "zap-address-1" "lud16"]]}]
          (should-throw Exception "conflicting zaps"
                        (zaps/get-zap-address event))))

      (it "accepts multiple zap tags that don't conflict"
        (let [event {:tags [[:zap "zap-address-1" "lud16"]
                            [:zap "zap-address-1" "lud16"]]}]
          (should= "zap-address-1" (zaps/get-zap-address event))))

      (it "only accepts lud16"
        (let [event {:tags [[:zap "zap-address" "lud06"]]}]
          (should-throw Exception "lud06 unimplemented"
                        (zaps/get-zap-address event))))

      (it "assumes an unspecified address type is lud16"
        (let [event {:tags [[:zap "zap-address"]]}]
          (should= "zap-address" (zaps/get-zap-address event))))
      )

    (context "No zap tag"
      (it "rejects if no profile for author"
        (let [event {:pubkey 1}]
          (should-throw Exception "no zap tag or profile"
                        (zaps/get-zap-address event))))

      (it "rejects if profile has no lud16"
        (let [event {:pubkey 1}]
          (gateway/add-profile @db 1 {:name "somebody"})
          (should-throw Exception "no lud16 in profile"
                        (zaps/get-zap-address event))))

      (it "gets zap addr from profile"
        (let [event {:pubkey 1}]
          (gateway/add-profile @db 1 {:name "somebody"
                                      :lud16 "zap-addr"})
          (should= "zap-addr" (zaps/get-zap-address event))))
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
                _ (reset! relays {"relay-r1" {:read :read-all}
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
          (should-throw Exception "Amount 100 is below minimum of 1000"
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
          (should-throw Exception "Amount 2000000 is larger than maximum of 1000000"
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
  )
