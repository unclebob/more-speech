(ns more-speech.ui.swing.profile-window-spec
  (:require
    [more-speech.bech32 :as bech32]
    [more-speech.config :as config]
    [more-speech.data-storage :as data-storage]
    [more-speech.db.in-memory :as in-memory]
    [more-speech.mem :refer :all]
    [more-speech.nostr.elliptic-signature :as es]
    [more-speech.nostr.event-composers :as event-composers]
    [more-speech.nostr.protocol :as protocol]
    [more-speech.nostr.util :as util]
    [more-speech.ui.swing.profile-window :refer :all]
    [speclj.core :refer :all]))

(declare db)
(describe "profile window"
  (with-stubs)
  (with db (in-memory/get-db))
  (before-all (config/set-db! :in-memory))
  (before (in-memory/clear-db @db))
  (before (clear-mem))

  (context "saving profile"
    (it "should save valid profile with hex private key"
      (with-redefs [event-composers/compose-and-send-metadata-event (stub :compose)
                    protocol/request-profiles-and-contacts-for (stub :request)
                    data-storage/write-keys (stub :write-keys)]
        (let [private-key (util/bytes->num (util/make-private-key))
              hex-private-key (util/hexify private-key)
              pubkey (->> private-key (util/num->bytes 32) es/get-pub-key util/bytes->num)
              hex-pubkey (util/hexify pubkey)]
          (save-profile "name" hex-private-key "about" "picture" "nip05" "lud16")
          (should= pubkey (get-mem :pubkey))
          (should= {:private-key hex-private-key
                    :public-key hex-pubkey
                    :name "name"
                    :about "about"
                    :picture "picture"
                    :nip05 "nip05"
                    :lud16 "lud16"}
                   (get-mem :keys))
          (should-have-invoked :write-keys {:with [(get-mem :keys)]})
          (Thread/sleep 10)
          (should-have-invoked :compose)
          (should-have-invoked :request))))

    (it "should save valid profile with nesec private key"
      (with-redefs [event-composers/compose-and-send-metadata-event (stub :compose)
                    protocol/request-profiles-and-contacts-for (stub :request)
                    data-storage/write-keys (stub :write-keys)]
        (let [private-key (util/bytes->num (util/make-private-key))
              hex-private-key (util/hexify private-key)
              nsec-private-key (bech32/encode "nsec" private-key)
              pubkey (->> private-key (util/num->bytes 32) es/get-pub-key util/bytes->num)
              hex-pubkey (util/hexify pubkey)]
          (save-profile "name" nsec-private-key "about" "picture" "nip05" "lud16")
          (should= pubkey (get-mem :pubkey))
          (should= {:private-key hex-private-key
                    :public-key hex-pubkey
                    :name "name"
                    :about "about"
                    :picture "picture"
                    :nip05 "nip05"
                    :lud16 "lud16"}
                   (get-mem :keys))
          (should-have-invoked :write-keys {:with [(get-mem :keys)]})
          (Thread/sleep 10)
          (should-have-invoked :compose)
          (should-have-invoked :request))))

    (it "should save valid profile with no private key change"
      (with-redefs [event-composers/compose-and-send-metadata-event (stub :compose)
                    protocol/request-profiles-and-contacts-for (stub :request)
                    data-storage/write-keys (stub :write-keys)]
        (set-mem [:keys :public-key] "pubkey")
        (set-mem [:keys :private-key] "privkey")
        (set-mem :pubkey "old-pubkey")
        (save-profile "name" "not-a-key" "about" "picture" "nip05" "lud16")
        (should= "old-pubkey" (get-mem :pubkey))
        (should= {:private-key "privkey"
                  :public-key "pubkey"
                  :name "name"
                  :about "about"
                  :picture "picture"
                  :nip05 "nip05"
                  :lud16 "lud16"}
                 (get-mem :keys))
        (should-have-invoked :write-keys {:with [(get-mem :keys)]})
        (Thread/sleep 10)
        (should-have-invoked :compose)
        (should-have-invoked :request)))
    )
  )
