(ns more-speech.nostr.trust-updater-spec
  (:require [more-speech.db.gateway :as gateway]
            [more-speech.mem :refer :all]
            [more-speech.nostr.trust-updater :refer :all]
            [more-speech.spec-util :refer :all]
            [speclj.core :refer :all]))

(declare db)
(describe "Setting Trust"
  (with-stubs)
  (setup-db-mem)

  (it "establishes new trust with a petname."
    (let [my-pubkey 1]
      (set-mem :pubkey my-pubkey)
      (gateway/add-contacts @db my-pubkey [{:pubkey 2}])
      (entrust 3 "the-petname")
      (let [my-contacts (gateway/get-contacts @db my-pubkey)]
        (should= (set [{:pubkey 2}
                       {:pubkey 3 :petname "the-petname"}])
                 (set my-contacts)))
      ))

  (it "restablishes trust with a new petname."
    (let [my-pubkey 1
          contacts [{:pubkey 1}
                    {:pubkey 3 :petname "the-old-petname"}
                    {:pubkey 2}]]
      (set-mem :pubkey my-pubkey)
      (gateway/add-contacts @db my-pubkey contacts)
      (entrust 3 "the-new-petname")
      (let [my-contacts (gateway/get-contacts @db my-pubkey)]
        (should= (set [{:pubkey 1}
                       {:pubkey 2}
                       {:pubkey 3 :petname "the-new-petname"}])
                 (set my-contacts)))))

  (it "untrusts a user"
    (let [my-pubkey 1
          contacts [{:pubkey 1}
                    {:pubkey 2}
                    {:pubkey 3 :petname "petname"}]]
      (set-mem :pubkey my-pubkey)
      (gateway/add-contacts @db my-pubkey contacts)
      (untrust 2)
      (let [my-contacts
            (gateway/get-contacts @db my-pubkey)]
        (should= (set [{:pubkey 1}
                       {:pubkey 3 :petname "petname"}])
                 (set my-contacts)))))
  )
