(ns more-speech.nostr.contact-list-spec
  (:require [more-speech.db.gateway :as gateway]
            [more-speech.mem :refer :all]
            [more-speech.nostr.contact-list :refer :all]
            [more-speech.nostr.util :refer [hexify]]
            [more-speech.spec-util :refer :all]
            [speclj.core :refer :all]))

(declare db)

(describe "contact-lists"
  (setup-db-mem)

  (describe "Processing Contact List events (Kind 3)"
    (it "processes and saves a full event"
      (let [pubkey 99
            contact1 1
            contact2 2
            event {:pubkey pubkey
                   :tags [[:p (hexify contact1) "relay1" "contact1"]
                          [:p (hexify contact2) "relay2" "contact2"]]}
            _ (process-contact-list @db event)]
        (should= [{:pubkey 1, :relay "relay1", :petname "contact1"}
                  {:pubkey 2, :relay "relay2", :petname "contact2"}]
                 (gateway/get-contacts @db pubkey))))

    (it "unpacks a full event"
      (let [pubkey 99
            contact1 1
            contact2 2
            event {:pubkey pubkey
                   :tags [[:p (hexify contact1) "relay1" "contact1"]
                          [:p (hexify contact2) "relay2" "contact2"]]}]
        (should= [pubkey
                  [{:pubkey contact1
                    :relay "relay1"
                    :petname "contact1"}
                   {:pubkey contact2
                    :relay "relay2"
                    :petname "contact2"}]]
                 (unpack-contact-list-event event))))

    (it "unpacks a partial event"
      (let [pubkey 99
            contact1 1
            contact2 2
            event {:pubkey pubkey
                   :tags [[:p (hexify contact1)]
                          [:p (hexify contact2)]]}]
        (should= [pubkey
                  [{:pubkey contact1
                    :relay nil
                    :petname nil}
                   {:pubkey contact2
                    :relay nil
                    :petname nil}]]
                 (unpack-contact-list-event event))))

    (it "properly skips a malformed tag"
      (let [pubkey 99
            contact2 2
            event {:pubkey pubkey
                   :tags [[:p "garbage"]
                          [:p (hexify contact2)]]}]
        (should= [pubkey
                  [{:pubkey contact2
                    :relay nil
                    :petname nil}]]
                 (unpack-contact-list-event event))))
    )

  (describe "Determining trust"
    (it "determines if a pubkey is trusted"
      (set-mem :pubkey 1)
      (gateway/add-contacts @db 1 [{:pubkey 2}])
      (should (is-trusted? 1))
      (should (is-trusted? 2))
      (should-not (is-trusted? 3)))

    (it "determines second degree trust"
      (let [my-pubkey 99
            trusted-user 1
            trusted-by-trusted-user 2]
        (set-mem :pubkey my-pubkey)
        (gateway/add-profile @db trusted-user {:name "trusted"})
        (gateway/add-profile @db trusted-by-trusted-user {:name "second-degree"})
        (gateway/add-contacts @db my-pubkey [{:pubkey trusted-user}])
        (gateway/add-contacts @db trusted-user [{:pubkey trusted-by-trusted-user}])
        (should= trusted-user (which-contact-trusts trusted-by-trusted-user))
        ))

    (it "gets my petname for a trusted user"
      (set-mem :pubkey 1)
      (gateway/add-contacts @db 1 [{:pubkey 2 :petname "two"}
                                   {:pubkey 3}])
      (should= "two" (get-petname 2))
      (should= nil (get-petname 3))
      (should= nil (get-petname 4)))

    (it "gets the pubkey for a trusted pet name"
      (set-mem :pubkey 1)
      (gateway/add-contacts @db 1 [{:pubkey 2 :petname "two"}
                                   {:pubkey 3}])

      (should= 2 (get-pubkey-from-petname "two"))
      (should= nil (get-pubkey-from-petname "none"))
      (should= nil (get-petname 3))
      (should= nil (get-petname 4)))
    )

  (describe "trust accessors"
    (setup-db-mem)

    (it "gets trustees"
      (set-mem :pubkey 99)
      (gateway/add-contacts @db 1 [{:pubkey 2} {:pubkey 3}])
      (should= #{99 2 3} (set (get-trustees 1))))

    (it "gets web of trust"
      (set-mem :pubkey 99)
      (gateway/add-contacts @db 1 [{:pubkey 2} {:pubkey 3}])
      (gateway/add-contacts @db 2 [{:pubkey 201} {:pubkey 202}])
      (gateway/add-contacts @db 3 [{:pubkey 201} {:pubkey 302}])
      (let [web-of-trust (get-web-of-trust 1)]
        (should= #{99 2 3 201 202 302} web-of-trust)))
    )

  )
