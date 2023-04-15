(ns more-speech.nostr.events_spec
  (:require [speclj.core :refer :all]
            [more-speech.db.gateway :as gateway]
            [more-speech.db.in-memory :as in-memory]
            [more-speech.nostr.events :refer :all]
            [more-speech.nostr.event-handlers :refer :all]
            [more-speech.nostr.elliptic-signature :refer :all]
            [more-speech.nostr.util :refer :all]
            [more-speech.mem :refer :all]
            [more-speech.config :as config]))

(defrecord event-handler-dummy []
  event-handler
  (handle-text-event [_ _event-id]))

(describe "process-tag(s)"
  (it "handles tags with many arguments"
    (should= [:e 1 2 3 4] (process-tag ["e" 1 2 3 4]))
    (should= [:e 1 2] (process-tag ["e" 1 2]))
    )
  (it "rejects tags without a valid tag name"
    (should-be-nil (process-tag ["" "" ""]))
    (should-be-nil (process-tag [nil])))
  (it "accumulates processed tags"
    (should= [[:e 1 2 3 4] [:e 1 2]] (process-tags [["e" 1 2 3 4] ["e" 1 2] ["" "" ""]])))
  )

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
          tags [["e:" 1 2 3] ["p:p:" 4 5 6 7] [" " "empty"]]
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
                :tags [[:e- 1 2 3] [:p-p- 4 5 6 7] [:blank "empty"]]
                :content content
                :sig sig}
               (translate-event event)))))

(declare now event db)
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
  (with db (in-memory/get-db))
  (before-all (config/set-db! :in-memory))
  (before
    (swap! (get-mem) assoc :event-handler (->event-handler-dummy))
    (in-memory/clear-events @db)
    (in-memory/clear-profiles @db))

  (it "adds one simple element"
    (process-text-event @db @event "url")
    (let [event (gateway/get-event @db 0xdeadbeef)]
      (should= 0xdeadbeef (:id event))
      (should= 0xf00d (:pubkey event))
      (should= @now (:created-at event))
      (should= "the content" (:content event))
      (should= 0xdddddd (:sig event))
      (should= #{"url"} (:relays event))
      (should= [[:p "0001" "someurl"]
                [:e "0002" "anotherurl"]] (:tags event))))

  (it "adds references to tagged articles."
    (gateway/add-event @db {:id 2})
    (add-cross-reference @db @event)
    (let [article (gateway/get-event @db 2)]
      (should= [0xdeadbeef] (:references article)))
    )

  (it "adds one event with two urls"
    (add-event @db {:id 10 :created-at 0} ["url1" "url2"])
    (let [event-10 (gateway/get-event @db 10)]
      (should= {:id 10 :created-at 0 :relays #{"url1" "url2"}}
               event-10)))

  (it "adds two elements with equal ids from two different relays"
    (add-event @db {:id 10 :created-at 1} ["url1"])
    (add-event @db {:id 10 :created-at 0} ["url2"])
    (let [event (gateway/get-event @db 10)]
      (should= #{"url1" "url2"} (:relays event)))))

(describe "relay recommendation event kind 2"
  (with now (int (/ (System/currentTimeMillis) 1000)))
  (it "adds a relay recommendation"
    (reset! relays {})
    (let [event {:id 1
                 :pubkey 1
                 :created-at @now
                 :kind 2
                 :tags []
                 :content "wss://relay-url"
                 :sig 0xdddddd}]
      (process-server-recommendation event)
      (should= {"wss://relay-url" {:read :read-none, :write false}} @relays))))

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

(describe "fixing names"
  (with db (in-memory/get-db))
  (before-all (config/set-db! :in-memory))
  (before (in-memory/clear-db @db))

  (it "should not fix a good name"
    (should= "name" (fix-name "name")))

  (it "should removed bad characters from a name."
    (should= "badname" (fix-name "bad name"))
    (should= "badname" (fix-name "bad.name")))

  (it "should create a random name for nils and empties"
    (with-redefs [rand-int (fn [_n] 12)]
      (should= "dud-12" (fix-name ""))))

  (it "should put a suffix on duplicate names."
    (gateway/add-profile @db 1 {:name "unclebob"})
    (let [new-name (add-suffix-for-duplicate 2 "unclebob")]
      (prn new-name)
      (should (re-matches #"unclebob\d+" new-name))))

  (it "should not put a suffix on previously existing names."
    (gateway/add-profile @db 1 {:name "unclebob"})
    (let [new-name (add-suffix-for-duplicate 1 "unclebob")]
      (should= "unclebob" new-name)))
  )

(describe "process-name-event"
  (with db (in-memory/get-db))
  (before-all (config/set-db! :in-memory))
  (before (in-memory/clear-db @db))

  (it "loads profiles"
    (process-name-event
      @db
      {:pubkey 1
       :created-at 1000
       :content (str "{\"name\": \"bob\""
                     ",\"about\": \"about\""
                     ",\"picture\": \"picture\""
                     ",\"lud06\": \"lud06\""
                     ",\"lud16\": \"lud16\""
                     ",\"nip05\": \"nip05\""
                     ",\"banner\": \"banner\""
                     ",\"website\": \"website\""
                     ",\"display_name\": \"display-name\""
                     "}")})
    (should= {:name "bob"
              :about "about"
              :picture "picture"
              :lud06 "lud06"
              :lud16 "lud16"
              :nip05 "nip05"
              :banner "banner"
              :website "website"
              :display-name "display-name"
              :created-at 1000}
             (gateway/get-profile @db 1)))

  (it "adds suffixes to duplicate names."
    (gateway/add-profile @db 7734 {:name "name"})
    (process-name-event
      @db
      {:pubkey 1
       :content "{\"name\": \"name\", \"about\": \"about\", \"picture\": \"picture\"}"})
    (let [{:keys [name about picture]} (gateway/get-profile @db 1)]
      (should= "about" about)
      (should= "picture" picture)
      (should (re-matches #"name\d+" name))))

  )

(declare body)
(describe "proof of work"
  (with body {:pubkey 1 :created_at 1 :kind 1 :tags [] :content "hi"})
  (it "makes id with no POW"
    (let [[_id new-body] (make-id-with-pow 0 @body)]
      (should= [[:nonce "0" "0"]] (:tags new-body))))

  (it "makes id with small POW"
    (let [[id new-body] (make-id-with-pow 4 @body)
          high-bits (subs (bytes->hex-string id) 0 1)
          [nonce-tag _nonce pow-promise] (first (:tags new-body))]
      (should= "0" high-bits)
      (should= 1 (:pubkey new-body))
      (should= 1 (:created_at new-body))
      (should= 1 (:kind new-body))
      (should= :nonce nonce-tag)
      (should= "4" pow-promise)
      (should= "hi" (:content new-body))))

  (it "makes id with larger POW"
    (let [[id new-body] (make-id-with-pow 8 @body)
          high-bits (subs (bytes->hex-string id) 0 2)
          [nonce-tag _nonce pow-promise] (first (:tags new-body))]
      (should= "00" high-bits)
      (should= 1 (:pubkey new-body))
      (should= 1 (:created_at new-body))
      (should= 1 (:kind new-body))
      (should= :nonce nonce-tag)
      (should= "8" pow-promise)
      (should= "hi" (:content new-body))))

  (it "makes id with even larger POW"
    (let [[id new-body] (make-id-with-pow 16 @body)
          high-bits (subs (bytes->hex-string id) 0 4)
          [nonce-tag _nonce pow-promise] (first (:tags new-body))]
      (should= "0000" high-bits)
      (should= 1 (:pubkey new-body))
      (should= 1 (:created_at new-body))
      (should= 1 (:kind new-body))
      (should= :nonce nonce-tag)
      (should= "16" pow-promise)
      (should= "hi" (:content new-body))))
  )

(describe "process=reaction"
  (with-stubs)
  (with db (in-memory/get-db))
  (before-all (config/set-db! :in-memory))
  (before (in-memory/clear-db @db))

  (it "adds a reaction to an event"
    (gateway/add-event @db {:id 1})
    (process-reaction @db {:pubkey 99 :content "!" :tags [[:e (hexify 1)] [:p (hexify 2)]]})
    (should= {:id 1 :reactions #{[99 "!"]}}
             (gateway/get-event @db 1)))

  (it "does not add a reaction if there is no event"
    (with-redefs [gateway/add-reaction (stub :add-reaction)]
      (process-reaction @db {:content "!" :tags [[:e (hexify 1)] [:p (hexify 2)]]})
      (should-not-have-invoked :add-reaction)))

  (it "chooses the last e and p tags"
    (gateway/add-event @db {:id 1})
    (process-reaction @db {:pubkey 99 :content "!" :tags [[:e "something"][:p "something"][:e (hexify 1)] [:p (hexify 2)]]})
    (should= {:id 1 :reactions #{[99 "!"]}}
             (gateway/get-event @db 1)))

  (it "does not add a reaction if no e tag"
    (gateway/add-event @db {:id 1})
    (with-redefs [gateway/add-reaction (stub :add-reaction)]
          (process-reaction @db {:content "!" :tags []})
          (process-reaction @db {:content "!" :tags [[:p (hexify 2)]]})
          (should-not-have-invoked :add-reaction)))
    )

(describe "tag processing"
  (context "tag extraction"
    (it "extracts empty list when target tag is not found"
      (should= [] (get-tag {} :tag))
      (should= [] (get-tag {:tags []} :tag))
      (should= [] (get-tag {:tags [[:e 1]]} :tag)))

    (it "extracts tag arguments when found"
      (should= [["arg1"]] (get-tag {:tags [[:target "arg1"]]} :target))
      (should= [["arg1" "arg2"]]
               (get-tag {:tags [[:target "arg1" "arg2"]]} :target))
      (should= [["arg1" "arg2"] ["arg3" "arg4"]]
                     (get-tag {:tags [[:e 1]
                                      [:target "arg1" "arg2"]
                                      [:p 2]
                                      [:target "arg3" "arg4"]
                                      [:q 3]]} :target))
      )

    ))
