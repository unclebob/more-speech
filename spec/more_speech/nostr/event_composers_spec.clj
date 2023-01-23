(ns more-speech.nostr.event-composers-spec
  (:require [speclj.core :refer :all]
            [more-speech.nostr.event-composers :refer :all]
            [more-speech.nostr.util :as util]
            [more-speech.nostr.events :refer :all]
            [more-speech.nostr.event-composers :refer :all]
            [more-speech.nostr.event-handlers :refer :all]
            [more-speech.nostr.elliptic-signature :refer :all]
            [more-speech.nostr.util :refer :all]
            [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.nostr.relays :refer [relays]]
            [more-speech.config :as config])
  (:import (ecdhJava SECP256K1)))

(defn have-client-tag? [tags]
  (let [[[tag-id tag-content]] tags]
    (and
      (= tag-id :client)
      (re-matches #"more\-speech \- [\d-:T]+" tag-content)))) ;allow #inst format

(describe "Composing outgoing events"
  (context "composing metadata (kind:0) messages"
    (it "composes using the keys data structure"
      (with-redefs [config/proof-of-work-default 0]
        (let [private-key (num->bytes 64 314159)
              public-key (get-pub-key private-key)
              keys {:private-key (bytes->hex-string private-key)
                    :public-key (bytes->hex-string public-key)
                    :name "name"
                    :about "about"
                    :picture "picture"}
              _ (reset! (:event-context @ui-context) {:keys keys})
              now (quot (System/currentTimeMillis) 1000)
              event (compose-metadata-event)
              {:keys [pubkey created_at kind content id sig]} (second event)
              ]
          (should= "EVENT" (first event))
          (should= (bytes->hex-string public-key) pubkey)
          (should (<= 0 (- now created_at) 10))             ;within ten seconds.
          (should= 0 kind)
          (should= (to-json {:name "name" :about "about" :picture "picture"}) content)
          (should (do-verify (hex-string->bytes id)
                             public-key
                             (hex-string->bytes sig))))
        ))
    )
  (context "composing Text (kind 1) messages"
    (it "composes an original message with no subject."
      (with-redefs [config/proof-of-work-default 0]
        (let [private-key (num->bytes 64 314159)
              public-key (get-pub-key private-key)
              _ (reset! (:event-context @ui-context) {:keys {:private-key (bytes->hex-string private-key)
                                                             :public-key (bytes->hex-string public-key)}})
              text "message text"
              subject ""
              event (compose-text-event subject text)
              {:keys [pubkey created_at kind tags content id sig]} (second event)
              now (quot (System/currentTimeMillis) 1000)]
          (should= "EVENT" (first event))
          (should= (bytes->hex-string public-key) pubkey)
          (should (<= 0 (- now created_at) 10))             ;within ten seconds.
          (should= 1 kind)
          (should (have-client-tag? tags))
          (should= text content)
          (should (do-verify (hex-string->bytes id)
                             public-key
                             (hex-string->bytes sig))))))

    (it "composes an original message with a subject."
      (with-redefs [config/proof-of-work-default 0]
        (let [private-key (num->bytes 64 314159)
              public-key (get-pub-key private-key)
              _ (reset! (:event-context @ui-context) {:keys {:private-key (bytes->hex-string private-key)
                                                             :public-key (bytes->hex-string public-key)}})
              text "message text"
              subject "subject"
              event (compose-text-event subject text)
              {:keys [pubkey created_at kind tags content id sig]} (second event)
              now (quot (System/currentTimeMillis) 1000)]
          (should= "EVENT" (first event))
          (should= (bytes->hex-string public-key) pubkey)
          (should (<= 0 (- now created_at) 10))             ;within ten seconds.
          (should= 1 kind)
          (should= [:subject "subject"] (first tags))
          (should= text content)
          (should (do-verify (hex-string->bytes id)
                             public-key
                             (hex-string->bytes sig))))))

    (it "composes a reply to a root article."
      (with-redefs [config/proof-of-work-default 0]
        (let [private-key (num->bytes 64 42)
              public-key (get-pub-key private-key)
              root-id 7734
              root-id-hex (hexify root-id)
              root-author 99
              _ (reset! (:event-context @ui-context) {:keys {:private-key (bytes->hex-string private-key)
                                                             :public-key (bytes->hex-string public-key)}
                                                      :text-event-map {root-id {:pubkey root-author
                                                                                :tags []}}
                                                      :pubkey public-key})
              text "message text"
              event (compose-text-event "" text root-id)
              {:keys [pubkey created_at kind tags content id sig]} (second event)
              now (quot (System/currentTimeMillis) 1000)]
          (should= "EVENT" (first event))
          (should= (bytes->hex-string public-key) pubkey)
          (should (<= 0 (- now created_at) 10))             ;within ten seconds.
          (should= 1 kind)
          (should= [[:e root-id-hex "" "reply"]
                    [:p (hexify root-author)]] (take 2 tags))
          (should= text content)
          (should (do-verify (hex-string->bytes id)
                             public-key
                             (hex-string->bytes sig))))))

    (it "composes a reply to a non-root article."
      (with-redefs [config/proof-of-work-default 0]
        (let [private-key (num->bytes 64 42)
              public-key (get-pub-key private-key)
              root-child-id 7734
              root-child-id-hex (hexify root-child-id)
              root-child-author 88
              root-id 1952
              root-id-hex (hexify root-id)
              root-author 99
              _ (reset! (:event-context @ui-context) {:keys {:private-key (bytes->hex-string private-key)
                                                             :public-key (bytes->hex-string public-key)}
                                                      :pubkey public-key
                                                      :text-event-map {root-child-id {:pubkey root-child-author
                                                                                      :tags [[:e root-id-hex]
                                                                                             [:p (hexify root-author)]]}
                                                                       root-id {:pubkey root-author
                                                                                :tags []}}})
              text "message text"
              event (compose-text-event "" text root-child-id)
              {:keys [pubkey created_at kind tags content id sig]} (second event)
              now (quot (System/currentTimeMillis) 1000)]
          (should= "EVENT" (first event))
          (should= (bytes->hex-string public-key) pubkey)
          (should (<= 0 (- now created_at) 10))             ;within ten seconds.
          (should= 1 kind)
          (should= [[:e root-id-hex "" "root"]
                    [:e root-child-id-hex "" "reply"]
                    [:p (hexify root-child-author)]
                    [:p (hexify root-author)]] (take 4 tags))
          (should= text content)
          (should (do-verify (hex-string->bytes id)
                             public-key
                             (hex-string->bytes sig))))))

    (it "author is removed from replies"
      (with-redefs [config/proof-of-work-default 0]
        (let [private-key (num->bytes 64 42)
              public-key (get-pub-key private-key)
              author (bytes->num public-key)
              root-id 7734
              root-id-hex (hexify root-id)
              root-author 99
              _ (reset! (:event-context @ui-context) {:keys {:private-key (bytes->hex-string private-key)
                                                             :public-key (bytes->hex-string public-key)}
                                                      :text-event-map {root-id {:pubkey root-author
                                                                                :tags [[:p (hexify author)]]}}
                                                      :pubkey public-key})
              event (compose-text-event "" "message" root-id)
              {:keys [tags]} (second event)]

          (should= [[:e root-id-hex "" "reply"]
                    [:p (hexify root-author)]] (take 2 tags)))))

    (it "composes a message with a slash."
      (with-redefs [config/proof-of-work-default 0]
        (let [private-key (num->bytes 64 42)
              public-key (get-pub-key private-key)
              _ (reset! (:event-context @ui-context) {:keys {:private-key (bytes->hex-string private-key)
                                                             :public-key (bytes->hex-string public-key)}})
              text "message/text"
              event (compose-text-event "" text)
              {:keys [pubkey created_at kind tags content id sig]} (second event)
              now (quot (System/currentTimeMillis) 1000)]
          (should= "EVENT" (first event))
          (should= (bytes->hex-string public-key) pubkey)
          (should (<= 0 (- now created_at) 10))             ;within ten seconds.
          (should= 1 kind)
          (should (have-client-tag? tags))
          (should= text content)
          (should (do-verify (hex-string->bytes id)
                             public-key
                             (hex-string->bytes sig))))))
    )

  (context "compose direct messages (kind 4)"
    (it "does not encrypt a regular message"
      (with-redefs [config/proof-of-work-default 0]
        (should= ["message" 1] (encrypt-if-direct-message "message" []))))

    (it "encrypts with shared keys"
      (with-redefs [config/proof-of-work-default 0]
        (let [sender-private-key (util/make-private-key)
              recipient-private-key (util/make-private-key)
              sender-public-key (get-pub-key sender-private-key)
              recipient-public-key (get-pub-key recipient-private-key)
              outbound-shared-secret (SECP256K1/calculateKeyAgreement
                                       (bytes->num sender-private-key)
                                       (bytes->num recipient-public-key))
              content "message"
              encrypted-content (SECP256K1/encrypt outbound-shared-secret content)
              inbound-shared-secret (SECP256K1/calculateKeyAgreement
                                      (bytes->num recipient-private-key)
                                      (bytes->num sender-public-key))]
          (should= inbound-shared-secret outbound-shared-secret)
          (should= content (SECP256K1/decrypt inbound-shared-secret encrypted-content)))))

    (it "encrypts a direct message"
      (with-redefs [config/proof-of-work-default 0]
        (let [event-context (:event-context @ui-context)
              sender-private-key (util/make-private-key)
              recipient-private-key (util/make-private-key)
              sender-public-key (get-pub-key sender-private-key)
              recipient-public-key (get-pub-key recipient-private-key)
              _ (reset! event-context {:keys {:private-key (bytes->hex-string sender-private-key)}})
              tags [[:p (bytes->hex-string recipient-public-key)]]
              content "D #[0] hi."
              inbound-shared-secret (SECP256K1/calculateKeyAgreement
                                      (bytes->num recipient-private-key)
                                      (bytes->num sender-public-key))
              [encrypted-message kind] (encrypt-if-direct-message content tags)]
          (should= 4 kind)
          (should= content (SECP256K1/decrypt inbound-shared-secret encrypted-message)))))

    (it "catches fake DMs with phoney #[xxx] in them."
      (with-redefs [config/proof-of-work-default 0]
        (let [event-context (:event-context @ui-context)
              sender-private-key (util/make-private-key)
              _ (reset! event-context {:keys {:private-key (bytes->num sender-private-key)}})
              tags [[:p "dummy"]]
              content "D #[223] hi."
              [encrypted-message kind] (encrypt-if-direct-message content tags)]
          (should= 1 kind)
          (should= content encrypted-message))))
    )

  (context "compose kind-3 contact-list event"
    (it "composes an simple contact list"
      (with-redefs [config/proof-of-work-default 0]
        (let [private-key (num->bytes 64 42)
              public-key (get-pub-key private-key)
              event-context (:event-context @ui-context)
              _ (reset! event-context {:keys {:private-key (bytes->hex-string private-key)
                                              :public-key (bytes->hex-string public-key)}})
              contact-list [{:pubkey 1}
                            {:pubkey 2 :petname "petname"}]
              event (compose-contact-list contact-list)
              {:keys [pubkey created_at kind tags content id sig]} (second event)
              now (quot (System/currentTimeMillis) 1000)
              ]
          (should= "EVENT" (first event))
          (should= (bytes->hex-string public-key) pubkey)
          (should (<= 0 (- now created_at) 10))             ;within ten seconds.
          (should= 3 kind)
          (should= "more-speech contact list" content)
          (should= [[:p (hexify 1) "" ""] [:p (hexify 2) "" "petname"]] (take 2 tags))
          (should (do-verify (hex-string->bytes id)
                             public-key
                             (hex-string->bytes sig))))))
    )
  )

(describe "Emplacing references"
  (context "replace @user with #[n] where n is the index of the 'p' tag."
    (it "emplaces nothing if there is no @user in the content"
      (let [tags []]
        (should= ["content" []] (emplace-references "content" tags))))

    (it "emplaces @username in newly created message"
      (let [tags []
            user-id 99
            profiles {user-id {:name "username"}}
            event-context (atom {:profiles profiles})
            _ (reset! ui-context {:event-context event-context})
            content "hello @username."]
        (should= ["hello #[0]." [[:p (hexify user-id)]]] (emplace-references content tags))))

    (it "emplaces two usernames in a message with some tags"
      (let [tags (seq [[:e "blah"]])
            user-id-1 99
            user-id-2 88
            profiles {user-id-1 {:name "user-1"}
                      user-id-2 {:name "user-2"}}
            event-context (atom {:profiles profiles})
            _ (reset! ui-context {:event-context event-context})
            content "hello @user-1 and @user-2."]
        (should= ["hello #[1] and #[2]." [[:e "blah"]
                                          [:p (hexify user-id-1)]
                                          [:p (hexify user-id-2)]]]
                 (emplace-references content tags))))

    (it "does not emplace a username that is not in the profile"
      (let [tags [[:e "blah"]]
            user-id-1 99
            user-id-2 88
            profiles {user-id-1 {:name "user-1"}
                      user-id-2 {:name "user-2"}}
            event-context (atom {:profiles profiles})
            _ (reset! ui-context {:event-context event-context})
            content "hello @user-3."]
        (should= ["hello @user-3." [[:e "blah"]]]
                 (emplace-references content tags))))

    (it "adds an abbreviated profile name for an unamed pubkey"
      (let [tags [[:e "blah"]]
            user-id 16r0123456789abcdef000000000000000000000000000000000000000000000000
            pubkey (num32->hex-string user-id)
            profiles {}
            event-context (atom {:profiles profiles})
            _ (reset! ui-context {:event-context event-context})
            content (str "hello @" pubkey ".")]
        (should= ["hello #[1]." [[:e "blah"] [:p pubkey]]]
                 (emplace-references content tags))
        (should= {user-id {:name "0123456789a-"}}
                 (:profiles @(:event-context @ui-context)))))

    (it "does not recognize pubkeys that aren't 32 bytes"
      (let [tags [[:e "blah"]]
            profiles {}
            event-context (atom {:profiles profiles})
            _ (reset! ui-context {:event-context event-context})
            content "hello @01234567abc."]
        (should= ["hello @01234567abc." [[:e "blah"]]]
                 (emplace-references content tags))
        (should= {} (:profiles @(:event-context @ui-context)))))))

(describe "find-user-id"
  (it "finds the id from a profile name"
    (let [event-state {:profiles {1 {:name "bob"}}}]
      (reset! ui-context {:event-context (atom event-state)})
      (should= 1 (find-user-id "bob"))
      (should= nil (find-user-id "bill"))))

  (it "finds the id from a trusted pet-name"
    (let [my-pubkey 1
          profiles {2 {:name "bob"}}
          contact-lists {my-pubkey [{:pubkey 2 :petname "petname"}]}
          event-state {:profiles profiles
                       :pubkey my-pubkey
                       :contact-lists contact-lists}]
      (reset! ui-context {:event-context (atom event-state)})
      (should= 2 (find-user-id "petname"))
      (should= 2 (find-user-id "bob"))))
  )