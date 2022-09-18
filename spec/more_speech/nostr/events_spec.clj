(ns more-speech.nostr.events_spec
  (:require [speclj.core :refer :all]
            [more-speech.nostr.util :as util]
            [more-speech.nostr.events :refer :all]
            [more-speech.nostr.elliptic-signature :refer :all]
            [more-speech.nostr.util :refer :all]
            [more-speech.ui.swing.ui-context :refer :all])
  (:import (ecdhJava SECP256K1)))

(defrecord event-handler-dummy []
  event-handler
  (handle-text-event [_ _event-id])
  )

(describe "process-tag"
  (it "handles tags with many arguments"
    (should= [:e 1 2 3 4] (process-tag ["e" 1 2 3 4]))
    (should= [:e 1 2] (process-tag ["e" 1 2]))
    ))

(describe "translate-event"
  (it "translates from strings to clojure values"
    (let [id (rand-int 1000000)
          pubkey (rand-int 1000000)
          sig (rand-int 1000000)
          created_at (rand-int 10000)
          content "the content"
          tags [["e" 1 2 3] ["p" 4 5 6 7]]
          event {"id" (hexify id)
                 "pubkey" (hexify pubkey)
                 "created_at" created_at
                 "kind" 1
                 "tags" tags
                 "content" content
                 "sig" (->> sig (num->bytes 64) bytes->hex-string)}]
      (should= {:id id
                :pubkey pubkey
                :created-at created_at
                :kind 1
                :tags [[:e 1 2 3] [:p 4 5 6 7]]
                :content content
                :sig sig}
               (translate-event event)))
    )

  (it "corrects malformed tags"
    (let [id (rand-int 1000000)
          pubkey (rand-int 1000000)
          sig (rand-int 1000000)
          created_at (rand-int 10000)
          content "the content"
          tags [["e:" 1 2 3] ["p:p:" 4 5 6 7]]
          event {"id" (hexify id)
                 "pubkey" (hexify pubkey)
                 "created_at" created_at
                 "kind" 1
                 "tags" tags
                 "content" content
                 "sig" (->> sig (num->bytes 64) bytes->hex-string)}]
      (should= {:id id
                :pubkey pubkey
                :created-at created_at
                :kind 1
                :tags [[:e- 1 2 3] [:p-p- 4 5 6 7]]
                :content content
                :sig sig}
               (translate-event event)))))

(declare now event state)
(describe "Processing Text events (Kind 1)"
  (with now (int (/ (System/currentTimeMillis) 1000)))
  (with event {:id 0xdeadbeef
               :pubkey 0xf00d
               :created-at @now
               :kind 1
               :tags [[:p "0001" "someurl"]
                      [:e "0002" "anotherurl"]]
               :content "the content"
               :sig 0xdddddd})
  (with state
        {:text-event-map {}
         :chronological-text-events (make-chronological-text-events)
         :event-handler (->event-handler-dummy)
         }
        )
  (it "creates the map of text events by id"
    (let [state (process-text-event @state @event "url")
          event-map (get-in state [:text-event-map])
          text-events (get-in state [:chronological-text-events])
          event (get event-map 0xdeadbeef :not-in-map)]
      (should= 1 (count event-map))
      (should= 1 (count text-events))
      (should= [0xdeadbeef @now] (first text-events))
      (should= [0xdeadbeef] (keys event-map))
      (should= 0xdeadbeef (:id event))
      (should= 0xf00d (:pubkey event))
      (should= @now (:created-at event))
      (should= "the content" (:content event))
      (should= 0xdddddd (:sig event))
      (should= ["url"] (:relays event))
      (should= [[:p "0001" "someurl"]
                [:e "0002" "anotherurl"]] (:tags event))))

  (it "adds references to tagged articles."
    (let [state (assoc-in @state
                          [:text-event-map 2]
                          {:id 2})
          state (process-references state @event)
          text-event-map (get-in state [:text-event-map])
          article (get text-event-map 2)]
      (should= [0xdeadbeef] (:references article)))
    )

  (context "sorted set for handling events"
    (it "adds one element"
      (let [state (add-event @state {:id 10 :created-at 0} "url")]
        (should= #{[10 0]} (get-in state [:chronological-text-events]))
        (should= {10 {:id 10 :created-at 0 :relays ["url"]}} (get-in state [:text-event-map]))))

    (it "adds two elements in chronological order, should be reversed"
      (let [state (add-event @state {:id 10 :created-at 0} "url")
            state (add-event state {:id 20 :created-at 1} "url")
            ]
        (should= [[20 1] [10 0]] (seq (get-in state [:chronological-text-events])))
        (should= {10 {:id 10 :created-at 0 :relays ["url"]}
                  20 {:id 20 :created-at 1 :relays ["url"]}} (get-in state [:text-event-map])))
      )
    (it "adds two elements in reverse chronological order, should remain."
      (let [state (add-event @state {:id 10 :created-at 1} "url")
            state (add-event state {:id 20 :created-at 0} "url")
            ]
        (should= [[10 1] [20 0]] (seq (get-in state [:chronological-text-events])))
        (should= {10 {:id 10 :created-at 1 :relays ["url"]}
                  20 {:id 20 :created-at 0 :relays ["url"]}}
                 (get-in state [:text-event-map])))
      )

    (it "adds two elements with equal ids from two different relays"
      (let [state (add-event @state {:id 10 :created-at 1} "url1")
            state (add-event state {:id 10 :created-at 0} "url2")
            event-map (get-in state [:text-event-map])
            event (get event-map 10)]
        (should= [[10 1]] (seq (get-in state [:chronological-text-events])))
        (should= 1 (count event-map))
        (should= ["url1" "url2"] (:relays event))
        )
      )
    )
  )

(defn have-client-tag? [tags]
  (let [[[tag-id tag-content]] tags]
    (and
      (= tag-id :client)
      (re-matches #"more\-speech \- \d+" tag-content))))

(describe "Composing outgoing events"
  (context "composing metadata (kind:0) messages"
    (it "composes using the keys data structure"
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
            {:keys [pubkey created_at kind tags content id sig]} (second event)
            ]
        (should= "EVENT" (first event))
        (should= (bytes->hex-string public-key) pubkey)
        (should (<= 0 (- now created_at) 1))                ;within one second.
        (should= 0 kind)
        (should= [] tags)
        (should= (to-json {:name "name" :about "about" :picture "picture"}) content)
        (should (do-verify (hex-string->bytes id)
                           public-key
                           (hex-string->bytes sig))))
      )
    )
  (context "composing Text (kind 1) messages"
    (it "composes an original message with no subject."
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
        (should (<= 0 (- now created_at) 1))                ;within one second.
        (should= 1 kind)
        (should (have-client-tag? tags))
        (should= text content)
        (should (do-verify (hex-string->bytes id)
                           public-key
                           (hex-string->bytes sig)))))

    (it "composes an original message with a subject."
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
        (should (<= 0 (- now created_at) 1))                ;within one second.
        (should= 1 kind)
        (should= [[:subject "subject"]] (drop-last tags))
        (should= text content)
        (should (do-verify (hex-string->bytes id)
                           public-key
                           (hex-string->bytes sig)))))

    (it "composes a reply to a root article."
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
        (should (<= 0 (- now created_at) 1))                ;within one second.
        (should= 1 kind)
        (should= [[:e root-id-hex "" "reply"]
                  [:p (hexify root-author)]] (drop-last tags))
        (should= text content)
        (should (do-verify (hex-string->bytes id)
                           public-key
                           (hex-string->bytes sig)))))

    (it "composes a reply to a non-root article."
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
        (should (<= 0 (- now created_at) 1))                ;within one second.
        (should= 1 kind)
        (should= [[:e root-id-hex "" "root"]
                  [:e root-child-id-hex "" "reply"]
                  [:p (hexify root-child-author)]
                  [:p (hexify root-author)]] (drop-last tags))
        (should= text content)
        (should (do-verify (hex-string->bytes id)
                           public-key
                           (hex-string->bytes sig)))))

    (it "author is removed from replies"
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
                  [:p (hexify root-author)]] (drop-last tags))))

    (it "composes a message with a slash."
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
        (should (<= 0 (- now created_at) 1))                ;within one second.
        (should= 1 kind)
        (should (have-client-tag? tags))
        (should= text content)
        (should (do-verify (hex-string->bytes id)
                           public-key
                           (hex-string->bytes sig)))))
    )

  (context "compose direct messages (kind 4)"
    (it "does not encrypt a regular message"
      (should= ["message" 1] (encrypt-if-direct-message "message" [])))

    (it "encrypts with shared keys"
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
        (should= content (SECP256K1/decrypt inbound-shared-secret encrypted-content))))

    (it "encrypts a direct message"
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
            (should= content (SECP256K1/decrypt inbound-shared-secret encrypted-message))))

    (it "catches fake DMs with phoney #[xxx] in them."
              (let [event-context (:event-context @ui-context)
                    sender-private-key (util/make-private-key)
                    recipient-private-key (util/make-private-key)
                    sender-public-key (get-pub-key sender-private-key)
                    _ (reset! event-context {:keys {:private-key (bytes->num sender-private-key)}})
                    tags [[:p "dummy"]]
                    content "D #[223] hi."
                    inbound-shared-secret (SECP256K1/calculateKeyAgreement
                                            (bytes->num recipient-private-key)
                                            (bytes->num sender-public-key))
                    [encrypted-message kind] (encrypt-if-direct-message content tags)]
                (should= 1 kind)
                (should= content encrypted-message)))
    )

  (context "compose kind-3 contact-list event"
    (it "composes an simple contact list"
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
        (should (<= 0 (- now created_at) 1))                ;within one second.
        (should= 3 kind)
        (should= "more-speech contact list" content)
        (should= [[:p (hexify 1) "" ""] [:p (hexify 2) "" "petname"]] tags)
        (should (do-verify (hex-string->bytes id)
                           public-key
                           (hex-string->bytes sig)))))
    )
  )

(describe "get references"
  (it "given no tags, finds no references"
    (let [event {:tags []}
          [root mentions referent] (get-references event)]
      (should-be-nil root)
      (should= [] mentions)
      (should-be-nil referent)))

  (it "given one tag, finds only the referent"
    (let [event {:tags [[:e (hexify 1)]]}
          [root mentions referent] (get-references event)]
      (should= 1 root)
      (should= [] mentions)
      (should= 1 referent)))

  (it "given two tags, finds root and referent"
    (let [event {:tags [[:e (hexify 1)]
                        [:e (hexify 2)]]}
          [root mentions referent] (get-references event)]
      (should= 1 root)
      (should= [] mentions)
      (should= 2 referent)))

  (it "given n>2 tags, finds root and referent"
    (let [event {:tags [[:e (hexify 1)]
                        [:e (hexify 2)]
                        [:e (hexify 3)]
                        [:e (hexify 4)]
                        [:e (hexify 5)]]}
          [root mentions referent] (get-references event)]
      (should= 1 root)
      (should= [2 3 4] mentions)
      (should= 5 referent)))

  (it "finds the root and reply when the tags are marked, irrespective of order."
    (let [event {:tags [[:e (hexify 1)]
                        [:e (hexify 2) "" "reply"]
                        [:e (hexify 3) "" "root"]
                        [:e (hexify 4) "" "wow"]]}
          [root mentions referent] (get-references event)]
      (should= 3 root)
      (should= 2 referent)
      (should= [1 4] mentions)))

  (it "finds the root and reply when only a reply is marked."
    (let [event {:tags [[:e (hexify 1)]
                        [:e (hexify 2) "" "reply"]
                        [:e (hexify 4) "" "wow"]]}
          [root mentions referent] (get-references event)]
      (should= 2 root)
      (should= 2 referent)
      (should= [1 4] mentions)))

  (it "finds the root when only root is marked."
    (let [event {:tags [[:e (hexify 2) "" "root"]]}
          [root mentions referent] (get-references event)]
      (should= 2 root)
      (should= 2 referent)
      (should= [] mentions)))
  )

(describe "json"
  (it "does not escape slashes"
    (should= "\"/\"" (to-json "/")))

  (it "does not escape unicode"
    (should= "\"Ω\"" (to-json "\u03a9")))

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

(describe "fixing names"
  (it "should not fix a good name"
    (should= "name" (fix-name "name")))

  (it "should removed bad characters from a name."
    (should= "badname" (fix-name "bad name"))
    (should= "badname" (fix-name "bad.name")))

  (it "should create a random name for nils and empties"
    (with-redefs [rand-int (fn [_n] 12)]
      (should= "dud-12" (fix-name ""))))

  (it "should put a suffix on duplicate names."
    (let [profiles {1 {:name "unclebob"}}
          event-context (atom {:profiles profiles})]
      (reset! ui-context {:event-context event-context})
      (let [new-name (add-suffix-for-duplicate 2 "unclebob")]
        (prn new-name)
        (should (re-matches #"unclebob\d+" new-name)))))

  (it "should put not put a suffix on previously existing names."
    (let [profiles {1 {:name "unclebob"}}
          event-context (atom {:profiles profiles})]
      (reset! ui-context {:event-context event-context})
      (let [new-name (add-suffix-for-duplicate 1 "unclebob")]
        (should= "unclebob" new-name))))
  )

(describe "process-name-event"
  (it "loads profiles"
    (let [event-state (process-name-event
                        {:profiles {}}
                        {:pubkey 1
                         :content "{\"name\": \"bob\", \"about\": \"about\", \"picture\": \"picture\"}"})]
      (should= {1 {:name "bob" :about "about" :picture "picture"}}
               (:profiles event-state)))))

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
