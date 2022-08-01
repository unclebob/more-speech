(ns more-speech.nostr.contact-list-spec
  (:require [speclj.core :refer :all]
            [more-speech.nostr.contact-list :refer :all]))

(describe "Processing Contact List events (Kind 3)"
  (it "unpacks an full event"
    (let [pubkey 99
          contact1 1
          contact2 2
          event {:pubkey pubkey :tags [[:p contact1 "relay1" "contact1"]
                                       [:p contact2 "relay2" "contact2"]]}]
      (should= [pubkey [{:pubkey contact1
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
          event {:pubkey pubkey :tags [[:p contact1]
                                       [:p contact2]]}]
      (should= [pubkey [{:pubkey contact1
                         :relay nil
                         :petname nil}
                        {:pubkey contact2
                         :relay nil
                         :petname nil}]]
               (unpack-contact-list-event event))))
  )