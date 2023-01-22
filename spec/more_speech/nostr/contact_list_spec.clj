(ns more-speech.nostr.contact-list-spec
  (:require [speclj.core :refer :all]
            [more-speech.db.gateway :as gateway]
            [more-speech.db.in-memory :as in-memory]
            [more-speech.nostr.contact-list :refer :all]
            [more-speech.nostr.util :as util]
            [more-speech.ui.swing.ui-context :refer :all]))

(defn hexify [n] (util/num32->hex-string n))

(describe "Processing Contact List events (Kind 3)"
  (it "processes and saves a full event"
    (let [db {:data (atom {}) ::gateway/type ::in-memory/type}
          pubkey 99
          contact1 1
          contact2 2
          event {:pubkey pubkey
                 :tags [[:p (hexify contact1) "relay1" "contact1"]
                        [:p (hexify contact2) "relay2" "contact2"]]}
          _ (process-contact-list db event)
          state @(:data db)]
      (should= {:contact-lists
                {99 [{:pubkey 1, :relay "relay1", :petname "contact1"}
                     {:pubkey 2, :relay "relay2", :petname "contact2"}]}}
               state)))

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
    (let [my-pubkey 1
          contact-lists {1 [{:pubkey 2}]}
          event-state {:pubkey my-pubkey :contact-lists contact-lists}]
      (reset! ui-context {:event-context (atom event-state)}))
    (should (is-trusted? 1))
    (should (is-trusted? 2))
    (should-not (is-trusted? 3)))

  (it "determines second degree trust"
    (let [my-pubkey 99
          trusted-user 1
          trusted-by-trusted-user 2
          profiles {trusted-user {:name "trusted"}
                    trusted-by-trusted-user {:name "second-degree"}}
          contact-lists {my-pubkey [{:pubkey trusted-user}]
                         trusted-user [{:pubkey trusted-by-trusted-user}]}
          event-state {:pubkey my-pubkey
                       :profiles profiles
                       :contact-lists contact-lists}]
      (reset! ui-context {:event-context (atom event-state)})
      (should= trusted-user (trusted-by-contact trusted-by-trusted-user))
      ))

  (it "gets my petname for a trusted user"
    (let [my-pubkey 1
          contact-lists {1 [{:pubkey 2 :petname "two"}
                            {:pubkey 3}]}
          event-state {:pubkey my-pubkey :contact-lists contact-lists}]
      (reset! ui-context {:event-context (atom event-state)}))
    (should= "two" (get-petname 2))
    (should= nil (get-petname 3))
    (should= nil (get-petname 4)))

  (it "gets the pubkey for a trusted pet name"
    (let [my-pubkey 1
          contact-lists {1 [{:pubkey 2 :petname "two"}
                            {:pubkey 3}]}
          event-state {:pubkey my-pubkey :contact-lists contact-lists}]
      (reset! ui-context {:event-context (atom event-state)}))
    (should= 2 (get-pubkey-from-petname "two"))
    (should= nil (get-pubkey-from-petname "none"))
    (should= nil (get-petname 3))
    (should= nil (get-petname 4)))
  )