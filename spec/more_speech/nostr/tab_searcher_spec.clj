(ns more-speech.nostr.tab-searcher-spec
  (:require [speclj.core :refer :all]
            [more-speech.nostr.tab-searcher :refer :all]
            [more-speech.db.gateway :as gateway]
            [more-speech.db.in-memory :as in-memory]
            [more-speech.config :as config]
            [more-speech.nostr.util :as util]
            [more-speech.mem :refer :all]))

(declare db)
(describe "Tab Searcher"
  (with-stubs)
  (with db (in-memory/get-db))
  (before-all (config/set-db! :in-memory))
  (before (in-memory/clear-db @db))

  (context "matching events to targets"
    (it "sees no match if target is not present"
      (let [event {:id 1N :pubkey 2N :tabs [] :content ""}]
        (should-not
          (match-target "target" event))))

    (it "should match if target is in content"
      (should
        (match-target "target" {:id 1N :pubkey 2N
                                :tags []
                                :content "the target is here"})))

    (it "should match if re-target is in content"
      (should
        (match-target "#\"t\\w+t\"" {:id 1N :pubkey 2N
                                     :tags []
                                     :content "the target is here"})))

    (it "should match if re-target is in subject"
      (should
        (match-target "#\"t\\w+t\"" {:id 1N :pubkey 2N
                                     :tags [[:e 1N] [:subject "the target is here"]]
                                     :content "nope"})))

    (it "should match if user name is target and user is embedded with #[n]"
      (gateway/add-profile @db 1N {:name "user-name"})
      (let [event {:id 99N :pubkey 98N :tags [[:p (util/hexify 1N)]]
                   :content "You are #[0]."}]
        (should (match-target "@user-name" event))))

    (it "should match if user name is target and user is author"
      (gateway/add-profile @db 1N {:name "user-name"})
      (let [event {:id 99N :pubkey 1N :tags []
                   :content "hi."}]
        (should (match-target "user-name" event))))

    (it "should match if petname is target and user is author"
      (set-mem :pubkey 99N)
      (gateway/add-profile @db 1N {:name "user-name"})
      (gateway/add-contacts @db 99N [{:pubkey 1N :petname "pet"}])
      (let [event {:id 99N :pubkey 1N :tags []
                   :content "hi."}]
        (should (match-target "pet" event))))

    (it "should match if hex number matches pubkey"
      (should (match-target (str (util/hexify 99N))
                            {:id 1N :pubkey 99N :content "" :tags []})))

    (it "should match if hex number matches id"
      (should (match-target (util/hexify 88N)
                            {:id 88N :pubkey 99N :content "" :tags []})))

    (it "should match if hex number matches any p tag"
      (should (match-target (util/hexify 88N)
                            {:id 1N :pubkey 2N :content ""
                             :tags [[:e (util/hexify 1N)]
                                    [:p (util/hexify 88N)]]})))

    (it "should match if npub matches any p tag"
      (should (match-target "npub1qzvsn7htrv"               ;153
                            {:id 153N :pubkey 2N :content ""
                             :tags [[:e (util/hexify 1N)]
                                    [:p (util/hexify 88N)]]})))

    (it "should match if petname matches any p tag"
      (set-mem :pubkey 99N)
      (gateway/add-contacts @db 99N [{:pubkey 1N :petname "pet"}])
      (should (match-target "pet"
                            {:id 153N :pubkey 2N :content ""
                             :tags [[:e (util/hexify 1N)]
                                    [:p (util/hexify 88N)]]})))
    )
  )
