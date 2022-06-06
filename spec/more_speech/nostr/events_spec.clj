(ns more-speech.nostr.events_spec
  (:require [speclj.core :refer :all]
            [more-speech.nostr.events :refer :all]
            [more-speech.nostr.elliptic-signature :refer :all]
            [more-speech.nostr.util :refer :all]
            [more-speech.ui.swing.ui-context :refer :all]))

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
    ))

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
            event-state {:keys keys}
            now (quot (System/currentTimeMillis) 1000)
            event (compose-metadata-event event-state)
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
            event-state {:keys {:private-key (bytes->hex-string private-key)}}
            public-key (get-pub-key private-key)
            text "message text"
            subject ""
            event (compose-text-event event-state subject text)
            {:keys [pubkey created_at kind tags content id sig]} (second event)
            now (quot (System/currentTimeMillis) 1000)]
        (should= "EVENT" (first event))
        (should= (bytes->hex-string public-key) pubkey)
        (should (<= 0 (- now created_at) 1))                ;within one second.
        (should= 1 kind)
        (should= [[:client "more-speech"]]tags)
        (should= text content)
        (should (do-verify (hex-string->bytes id)
                           public-key
                           (hex-string->bytes sig)))))

    (it "composes an original message with a subject."
      (let [private-key (num->bytes 64 314159)
            event-state {:keys {:private-key (bytes->hex-string private-key)}}
            public-key (get-pub-key private-key)
            text "message text"
            subject "subject"
            event (compose-text-event event-state subject text)
            {:keys [pubkey created_at kind tags content id sig]} (second event)
            now (quot (System/currentTimeMillis) 1000)]
        (should= "EVENT" (first event))
        (should= (bytes->hex-string public-key) pubkey)
        (should (<= 0 (- now created_at) 1))                ;within one second.
        (should= 1 kind)
        (should= [[:subject "subject"] [:client "more-speech"]] tags)
        (should= text content)
        (should (do-verify (hex-string->bytes id)
                           public-key
                           (hex-string->bytes sig)))))

    (it "composes a reply to a root article."
      (let [private-key (num->bytes 64 42)
            root-id 7734
            root-id-hex (hexify root-id)
            root-author 99
            event-state {:keys {:private-key (bytes->hex-string private-key)}
                         :text-event-map {root-id {:pubkey root-author
                                                   :tags []}}}
            public-key (get-pub-key private-key)
            text "message text"
            event (compose-text-event event-state "" text root-id)
            {:keys [pubkey created_at kind tags content id sig]} (second event)
            now (quot (System/currentTimeMillis) 1000)]
        (should= "EVENT" (first event))
        (should= (bytes->hex-string public-key) pubkey)
        (should (<= 0 (- now created_at) 1))                ;within one second.
        (should= 1 kind)
        (should= [[:e root-id-hex "" "reply"]
                  [:p (hexify root-author)]
                  [:client "more-speech"]] tags)
        (should= text content)
        (should (do-verify (hex-string->bytes id)
                           public-key
                           (hex-string->bytes sig)))))

    (it "composes a reply to a non-root article."
      (let [private-key (num->bytes 64 42)
            root-child-id 7734
            root-child-id-hex (hexify root-child-id)
            root-child-author 88
            root-id 1952
            root-id-hex (hexify root-id)
            root-author 99
            event-state {:keys {:private-key (bytes->hex-string private-key)}
                         :text-event-map {root-child-id {:pubkey root-child-author
                                                         :tags [[:e root-id-hex]
                                                                [:p (hexify root-author)]]}
                                          root-id {:pubkey root-author
                                                   :tags []}}}
            public-key (get-pub-key private-key)
            text "message text"
            event (compose-text-event event-state "" text root-child-id)
            {:keys [pubkey created_at kind tags content id sig]} (second event)
            now (quot (System/currentTimeMillis) 1000)]
        (should= "EVENT" (first event))
        (should= (bytes->hex-string public-key) pubkey)
        (should (<= 0 (- now created_at) 1))                ;within one second.
        (should= 1 kind)
        (should= [[:e root-id-hex "" "root"]
                  [:e root-child-id-hex "" "reply"]
                  [:p (hexify root-child-author)]
                  [:p (hexify root-author)]
                  [:client "more-speech"]] tags)
        (should= text content)
        (should (do-verify (hex-string->bytes id)
                           public-key
                           (hex-string->bytes sig)))))

    (it "author is removed from replies"
      (let [private-key (num->bytes 64 42)
            author (bytes->num (get-pub-key private-key))
            root-id 7734
            root-id-hex (hexify root-id)
            root-author 99
            event-state {:keys {:private-key (bytes->hex-string private-key)}
                         :text-event-map {root-id {:pubkey root-author
                                                   :tags [[:p (hexify author)]]}}}
            event (compose-text-event event-state "" "message" root-id)
            {:keys [tags]} (second event)]

        (should= [[:e root-id-hex "" "reply"]
                  [:p (hexify root-author)]
                  [:client "more-speech"]] tags)))

    (it "composes a message with a slash."
      (let [private-key (num->bytes 64 42)
            event-state {:keys {:private-key (bytes->hex-string private-key)}}
            public-key (get-pub-key private-key)
            text "message/text"
            event (compose-text-event event-state "" text)
            {:keys [pubkey created_at kind tags content id sig]} (second event)
            now (quot (System/currentTimeMillis) 1000)]
        (should= "EVENT" (first event))
        (should= (bytes->hex-string public-key) pubkey)
        (should (<= 0 (- now created_at) 1))                ;within one second.
        (should= 1 kind)
        (should= [[:client "more-speech"]] tags)
        (should= text content)
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
            nicknames {user-id "username"}
            event-context (atom {:nicknames nicknames})
            _ (reset! ui-context {:event-context event-context})
            content "hello @username."]
        (should= ["hello #[0]." [[:p (hexify user-id)]]] (emplace-references content tags))))

    (it "emplaces two usernames in a message with some tags"
      (let [tags (seq [[:e "blah"]])
            user-id-1 99
            user-id-2 88
            nicknames {user-id-1 "user-1"
                       user-id-2 "user-2"}
            event-context (atom {:nicknames nicknames})
            _ (reset! ui-context {:event-context event-context})
            content "hello @user-1 and @user-2."]
        (should= ["hello #[1] and #[2]." [[:e "blah"]
                                          [:p (hexify user-id-1)]
                                          [:p (hexify user-id-2)]]]
                 (emplace-references content tags))))

    (it "does not emplace a username that is not a nickname"
          (let [tags [[:e "blah"]]
                user-id-1 99
                user-id-2 88
                nicknames {user-id-1 "user-1"
                           user-id-2 "user-2"}
                event-context (atom {:nicknames nicknames})
                _ (reset! ui-context {:event-context event-context})
                content "hello @user-3."]
            (should= ["hello @user-3." [[:e "blah"]]]
                     (emplace-references content tags))))
    )

  )
