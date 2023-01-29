(ns more-speech.nostr.trust-updater-spec
  (:require [speclj.core :refer :all]
            [more-speech.nostr.trust-updater :refer :all]
            [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.db.in-memory :as in-memory]
            [more-speech.db.gateway :as gateway]))

(declare db)
(describe "Setting Trust"
  (with-stubs)
  (with db (in-memory/get-db))
  (before (in-memory/clear-db @db))

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
                    {:pubkey 2}]
          ]
      (set-mem :pubkey my-pubkey)
      (gateway/add-contacts @db my-pubkey contacts)
      (entrust 3 "the-new-petname")
      (let [my-contacts
            (gateway/get-contacts @db my-pubkey)
            ]
        (should= (set [{:pubkey 1}
                       {:pubkey 2}
                       {:pubkey 3 :petname "the-new-petname"}])
                 (set my-contacts)))))
  )
