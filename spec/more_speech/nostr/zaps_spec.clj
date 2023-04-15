(ns more-speech.nostr.zaps-spec
  (:require
    [more-speech.config :as config]
    [more-speech.db.gateway :as gateway]
    [more-speech.db.in-memory :as in-memory]
    [more-speech.mem :refer :all]
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
                 (zaps/lud16->lnurl "name@domain.xxx"))
        ))
    )
  )
