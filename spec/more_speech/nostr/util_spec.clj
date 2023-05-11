(ns more-speech.nostr.util-spec
  (:require
    [clojure.test.check :as tc]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [more-speech.nostr.util :refer :all]
    [speclj.core :refer :all]))

(describe "Hex string and bytes conversions"
  (it "decodes chars"
    (should= 16rab (Integer/parseInt "ab" 16)))

  (it "does hex-decode"
    (should= "deadbeef" (bytes->hex-string (hex-string->bytes "deadbeef")))
    (should= "aff9a9f017f32b2e8b60754a4102db9d9cf9ff2b967804b50e070780aa45c9a8"
             (bytes->hex-string (hex-string->bytes "aff9a9f017f32b2e8b60754a4102db9d9cf9ff2b967804b50e070780aa45c9a8")))
    )

  (it "converts byte arrays to strings"
    (should= "abcdef" (bytes->hex-string (hex-string->bytes "abcdef"))))

  (it "xors bytes"
    (should= "b2ce" (bytes->hex-string (xor-bytes (hex-string->bytes "af01")
                                                  (hex-string->bytes "1dcf"))))
    (should= "000000000000"
             (bytes->hex-string
               (xor-bytes (hex-string->bytes "deadbeeffeed")
                          (hex-string->bytes "deadbeeffeed"))))
    (should= "ffffffffffff"
             (bytes->hex-string
               (xor-bytes (hex-string->bytes "aaaacccc3333")
                          (hex-string->bytes "55553333cccc"))))
    )

  (it "should convert numbers to bytes"
    (should= "1fcde563"
             (bytes->hex-string
               (num->bytes 4
                           (bytes->num
                             (hex-string->bytes "1fcde563")))))

    (should= "ffcde563"
             (bytes->hex-string
               (num->bytes 4
                           (bytes->num
                             (hex-string->bytes "ffcde563")))))
    )
  )

(def gen-string (gen/such-that not-empty gen/string-alphanumeric))

(describe "xor-strings"
  (it "xors a string"
    (should= "pspspspsp" (xor-string "1212" "AAAAAAAAA"))
    (should= "AAAAAAAAA" (xor-string "1212" "pspspspsp")))

  (it "xors strings"
    (should-be
      :result
      (tc/quick-check
        1000
        (prop/for-all
          [source gen-string
           pw gen-string]
          (= source (->> source (xor-string pw) (xor-string pw)))
          ))))
  )
