(ns more-speech.nostr.ellliptic-signature-spec
  (:require [speclj.core :refer :all]
            [more-speech.nostr.elliptic-signature :refer :all])
  (:import (java.nio.charset StandardCharsets)))

(describe "verification"
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

  (it "signs"
    (let [private-key-1 (sha-256 (.getBytes "my private key 1" StandardCharsets/UTF_8))
          private-key-2 (sha-256 (.getBytes "my private key 2" StandardCharsets/UTF_8))
          public-key-1 (pub-key private-key-1)
          public-key-2 (pub-key private-key-2)
          message-1 (sha-256 (.getBytes "my message 1" StandardCharsets/UTF_8))
          message-2 (sha-256 (.getBytes "my message 2" StandardCharsets/UTF_8))
          sig-1 (sign private-key-1 message-1)
          sig-2 (sign private-key-2 message-2)
          ]
      (should= true (verify public-key-1 message-1 sig-1))
      (should= true (verify public-key-2 message-2 sig-2))
      (should-not (verify public-key-1 message-1 sig-2))
      (should-not (verify public-key-2 message-2 sig-1))
      (should-not (verify public-key-1 message-2 sig-1))
      (should-not (verify public-key-2 message-1 sig-2))))

  (it "verifies Aaron Dixon's example."
    (should= true
             (do-verify
               (hex-string->bytes "2a3a85a53e99af51eb6b3303fb4c902594827f4c9da0a183f743054a9e3d3a33")
               (hex-string->bytes "aff9a9f017f32b2e8b60754a4102db9d9cf9ff2b967804b50e070780aa45c9a8")
               (hex-string->bytes "0e049520f7683ab9ca1c3f50243e09c11ace8fcabb0e2fcdd80861b9802d83180ca0bed00f42a032ac780152a3b3a5c01c136a271a7d360379a1f29e13eceb9d"))))

  (it "should verify a known good signature"
    (let [pubkey (hex-string->bytes "2ef93f01cd2493e04235a6b87b10d3c4a74e2a7eb7c3caf168268f6af73314b5")
          id (hex-string->bytes "c60fb1c88baceb9d7cfd84de8c7f92e1d4d6690e6cb65b97405e72a74acc5214")
          sig (hex-string->bytes "e5c4dfda7046f99469594f7bfd236f6e285be187aa7f604bff533636871d523f58ccb85169310a49b06a9499fd30d5a5806e4044e650cfd8eaef5b95597d1427")]
      (should (do-verify id pubkey sig))))

  (context "Standard Test Vector"
    (it "passes index 0"
      (let [private-key (hex-string->bytes "0000000000000000000000000000000000000000000000000000000000000003")
            public-key (hex-string->bytes "F9308A019258C31049344F85F89D5229B531C845836F99B08601F113BCE036F9")
            aux-rand (hex-string->bytes "0000000000000000000000000000000000000000000000000000000000000000")
            message (hex-string->bytes "0000000000000000000000000000000000000000000000000000000000000000")
            signature (hex-string->bytes "E907831F80848D1069A5371B402410364BDF1C5F8307B0084C55F1CE2DCA821525F66A4A85EA8B71E482A74F382D2CE5EBEEE8FDB2172F477DF4900D310536C0")]
        (should (bytes= public-key (pub-key private-key)))
        (should (bytes= signature (do-sign message private-key aux-rand)))
        (should (do-verify message public-key signature))
        ))
    (it "passes index 1"
      (let [private-key (hex-string->bytes "B7E151628AED2A6ABF7158809CF4F3C762E7160F38B4DA56A784D9045190CFEF")
            public-key (hex-string->bytes "DFF1D77F2A671C5F36183726DB2341BE58FEAE1DA2DECED843240F7B502BA659")
            aux-rand (hex-string->bytes "0000000000000000000000000000000000000000000000000000000000000001")
            message (hex-string->bytes "243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89")
            signature (hex-string->bytes "6896BD60EEAE296DB48A229FF71DFE071BDE413E6D43F917DC8DCF8C78DE33418906D11AC976ABCCB20B091292BFF4EA897EFCB639EA871CFA95F6DE339E4B0A")]
        (should (bytes= public-key (pub-key private-key)))
        (should (bytes= signature (do-sign message private-key aux-rand)))
        (should (do-verify message public-key signature))
        ))
    (it "passes index 2"
      (let [private-key (hex-string->bytes "C90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B14E5C9")
            public-key (hex-string->bytes "DD308AFEC5777E13121FA72B9CC1B7CC0139715309B086C960E18FD969774EB8")
            aux-rand (hex-string->bytes "C87AA53824B4D7AE2EB035A2B5BBBCCC080E76CDC6D1692C4B0B62D798E6D906")
            message (hex-string->bytes "7E2D58D8B3BCDF1ABADEC7829054F90DDA9805AAB56C77333024B9D0A508B75C")
            signature (hex-string->bytes "5831AAEED7B44BB74E5EAB94BA9D4294C49BCF2A60728D8B4C200F50DD313C1BAB745879A5AD954A72C45A91C3A51D3C7ADEA98D82F8481E0E1E03674A6F3FB7")]
        (should (bytes= public-key (pub-key private-key)))
        (should (bytes= signature (do-sign message private-key aux-rand)))
        (should (do-verify message public-key signature))
        ))

    )
  )

