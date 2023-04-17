(ns more-speech.bech32-spec
  (:require [more-speech.bech32 :refer :all]
            [more-speech.nostr.util :as util]
            [speclj.core :refer :all]))

(describe "bech32"
  (context "charset translations"
    (it "should translate charset chars to five bit numbers"
      (should= 0 (to-n \q))
      (should= 31 (to-n \l))
      (should= -1 (to-n \.)))

    (it "should translate integers to charset chars"
      (should= \q (to-char 0))
      (should= \l (to-char 31))
      (should= nil (to-char 32))
      (should= nil (to-char -1))))

  (context "parsing bech32 address"
    (it "parses address syntax"
      (should= ["a" "" "2uel5l"] (parse-address "A12UEL5L"))
      (should= ["a" "" "2uel5l"] (parse-address "a12uel5l"))
      (should= ["an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio" "" "tt5tgs"]
               (parse-address "an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs"))
      (should= ["abcdef" "qpzry9x8gf2tvdw0s3jn54khce6mua7l" "mqqqxw"]
               (parse-address "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw"))
      (should= ["1" "qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq" "c8247j"]
               (parse-address "11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j"))
      (should= ["split" "checkupstagehandshakeupstreamerranterredcaperred" "2y9e3w"]
               (parse-address "split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w"))
      (should= ["?" "" "ezyfcl"]
               (parse-address "?1ezyfcl"))

      (should-throw Exception "bech32: hrp invalid char"
                    (parse-address (str (char 0x20) "1nwldj5")))
      (should-throw Exception "bech32: hrp invalid char"
                    (parse-address (str (char 0x7f) "1axkwrx")))
      (should-throw Exception "bech32: hrp invalid char"
                    (parse-address (str (char 0x80) "1eym55h")))
      (should-throw Exception "bech32: address too long"
                    (parse-address "an84characterslonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1569pvx:"))
      (should-throw Exception "bech32: no separator character (1)"
                    (parse-address "pzry9x0s0muk"))
      (should-throw Exception "bech32: no hrp"
                    (parse-address "1pzry9x0s0muk"))
      (should-throw Exception "bech32: invalid data character"
                    (parse-address "x1b4n0q5v"))
      (should-throw Exception "bech32: checksum too short"
                    (parse-address "li1dgmt3"))
      (should-throw Exception "bech32: invalid checksum character"
                    (parse-address (str "de1lg7wt" (char 0xff))))))

  (context "validating checksum"
    (it "computes expanded hrp"
      (should= [3, 3, 3, 3, 0, 14, 16, 21, 2]
               (hrp-expand "npub")))

    (it "validates checksums"
      (should (verify-checksum? (parse-address "A12UEL5L")))
      (should (verify-checksum? (parse-address "a12uel5l")))
      (should (verify-checksum? (parse-address "an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs")))
      (should (verify-checksum? (parse-address "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw")))
      (should (verify-checksum? (parse-address "11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j")))
      (should (verify-checksum? (parse-address "split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w")))
      (should (verify-checksum? (parse-address "?1ezyfcl")))
      (should (verify-checksum? (parse-address "npub1sg6plzptd64u62a878hep2kev88swjh3tw00gjsfl8f237lmu63q0uf63m")))

      (should-not (verify-checksum? (parse-address "A1G7SGD8")))))

  (context "conversions between numbers and addresses"
    (it "converts addresses to numbers"
      (should= 0x82341f882b6eabcd2ba7f1ef90aad961cf074af15b9ef44a09f9d2a8fbfbe6a2
               (address->number "npub1sg6plzptd64u62a878hep2kev88swjh3tw00gjsfl8f237lmu63q0uf63m")))

    (it "converts numbers to addresses"
      (should= "npub1sg6plzptd64u62a878hep2kev88swjh3tw00gjsfl8f237lmu63q0uf63m"
               (encode "npub" 0x82341f882b6eabcd2ba7f1ef90aad961cf074af15b9ef44a09f9d2a8fbfbe6a2)))

    (it "can encode smaller numbers"
      (should= 32768 (address->number (encode "xxx" 32768)))
      (should= "xxx1sqqqh8ke4q" (encode "xxx" 32768)))

    (it "encodes my public key"
      (let [pubkey (util/hex-string->num "2ef93f01cd2493e04235a6b87b10d3c4a74e2a7eb7c3caf168268f6af73314b5")]
        (should= "npub19mun7qwdyjf7qs3456u8kyxncjn5u2n7klpu4utgy68k4aenzj6synjnft"
                 (encode "npub" pubkey))
        (should= pubkey (address->number "npub19mun7qwdyjf7qs3456u8kyxncjn5u2n7klpu4utgy68k4aenzj6synjnft")))
      )
    )

  (context "decoding into strings"
    (it "decodes into a string"
      (let [lnurl (encode-str "lnurl" "this is the string")]
        (should= "this is the string" (address->str lnurl))))

    (it "decodes a long string"
      (let [long-string "https://service.com/api?q=3fc3645b439ce8e7f2553a69e5267081d96dcd340693afabe04be7b0ccd178df"
            lnurl (encode-str "lnurl" long-string)]
        (should= long-string (address->str lnurl))))

    (it "encodes and decodes a one byte string"
           (let [known-string "t"
                 known-lnurl "lnurl1ws4pqzkn"]
             (should= known-lnurl (encode-str "lnurl" known-string))
             (should= known-string (address->str known-lnurl))))

    (it "encodes and decodes a known short string"
          (let [known-string "this is the string"
                known-lnurl "lnurl1w35xjueqd9ejqargv5s8xarjd9hxw9sar9p"]
            (should= known-lnurl (encode-str "lnurl" known-string))
            (should= known-string (address->str known-lnurl))))

    (it "encodes and decodes a known long string"
      (let [known-string "https://service.com/api?q=3fc3645b439ce8e7f2553a69e5267081d96dcd340693afabe04be7b0ccd178df"
            known-lnurl "lnurl1dp68gurn8ghj7um9wfmxjcm99e3k7mf0v9cxj0m385ekvcenxc6r2c35xvukxefcv5mkvv34x5ekzd3ev56nyd3hxqurzepexejxxepnxscrvwfnv9nxzcn9xq6xyefhvgcxxcmyxymnserxfq5fns"]
        (should= known-string (address->str known-lnurl))
        (should= known-lnurl (encode-str "lnurl" known-string))))
    )
  )