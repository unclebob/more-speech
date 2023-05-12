(ns more-speech.migrator-spec
  (:require [clojure.java.io :as io]
            [more-speech.bech32 :as bech32]
            [more-speech.config :as config]
            [more-speech.data-storage :as data-storage]
            [more-speech.db.gateway :as gateway]
            [more-speech.db.in-memory :as in-memory]
            [more-speech.mem :as mem]
            [more-speech.migrator :refer :all]
            [more-speech.nostr.elliptic-signature :as es]
            [more-speech.nostr.util :as util]
            [more-speech.user-configuration :as user-configuration]
            [more-speech.util.files :refer :all]
            [speclj.core :refer :all]))

(defn change-to-tmp-files []
  (when (file-exists? "tmp")
    (delete-dir "tmp"))
  (reset! config/private-directory "tmp")
  (reset! config/migration-filename "tmp/migration")
  (reset! config/nicknames-filename "tmp/nicknames")        ;grandfathered
  (reset! config/profiles-filename "tmp/profiles")
  (reset! config/keys-filename "tmp/keys")
  (reset! config/relays-filename "tmp/relays")
  (reset! config/read-event-ids-filename "tmp/read-event-ids")
  (reset! config/tabs-filename "tmp/tabs")
  (reset! config/tabs-list-filename "tmp/tabs-list")
  (reset! config/messages-directory "tmp/messages")
  (reset! config/messages-filename "tmp/messages/message-file")
  (reset! config/user-configuration-filename "tmp/user-configuration")
  (reset! config/contact-lists-filename "tmp/contact-lists")
  (.mkdir (io/file "tmp"))
  (prn 'changed-to-tmp)
  )

(defn revert-from-tmp []
  (delete-dir "tmp")
  (reset! config/private-directory "private")
  (reset! config/migration-filename "private/migration")
  (reset! config/nicknames-filename "private/nicknames")    ;grandfathered
  (reset! config/profiles-filename "private/profiles")
  (reset! config/keys-filename "private/keys")
  (reset! config/relays-filename "private/relays")
  (reset! config/read-event-ids-filename "private/read-event-ids")
  (reset! config/tabs-filename "private/tabs")
  (reset! config/tabs-list-filename "private/tabs-list")
  (reset! config/messages-directory "private/messages")
  (reset! config/messages-filename "private/messages/message-file")
  (reset! config/user-configuration-filename "private/user-configuration")
  (reset! config/contact-lists-filename "private/contact-lists")
  )

(declare db)

(describe "The Migrator"
  (with-stubs)
  (before-all (change-to-tmp-files))
  (after (delete-dir "tmp")
         (.mkdir (io/file "tmp")))
  (after-all (revert-from-tmp))

  (context "the migration framework"

    (it "returns zero if no migration file"
      (should= 0 (get-migration-level)))

    (it "reads the migration level"
      (should (file-exists? "tmp"))
      (set-migration-level 42)
      (should= 42 (get-migration-level)))

    (it "determines migrations to perform if no migration file"
      (should= [1 2 3 4] (get-needed-migrations 4)))

    (it "determines migrations to perform if level already set"
      (set-migration-level 10)
      (should= [11 12 13 14 15] (get-needed-migrations 15)))

    (it "throws exception if set level is greater than needed."
      (set-migration-level 10)
      (should-throw (get-needed-migrations 9)))

    (it "should not execute migrations if at current level"
      (set-migration-level 1)
      (reset! migrations {1 (stub :migration-one)})
      (migrate-to 1)
      (should-not-have-invoked :migration-one)
      (should= 1 (get-migration-level)))

    (it "should execute appropriate migrations if not at current level"
      (set-migration-level 1)
      (reset! migrations {1 (stub :migration-one)
                          2 (stub :migration-two)
                          3 (stub :migration-three)
                          4 (stub :migration-four)})
      (migrate-to 3)
      (should-not-have-invoked :migration-one)
      (should-have-invoked :migration-two)
      (should-have-invoked :migration-three)
      (should-not-have-invoked :migration-four)
      (should= 3 (get-migration-level)))

    (it "should complain if a migration function is missing"
      (set-migration-level 1)
      (reset! migrations {1 (stub :migration-one)
                          2 (stub :migration-two)
                          4 (stub :migration-four)})
      (should-throw Exception "Missing migrations [3]." (migrate-to 4))
      (should-not-have-invoked :migration-one)
      (should-not-have-invoked :migration-two)
      (should-not-have-invoked :migration-four))
    )

  (context "The initial migration"
    (it "creates all necessary files and warns about the key file if not present."
      (initial-migration)
      (should (file-exists? @config/keys-filename))
      (should (file-exists? @config/nicknames-filename))
      (should (file-exists? @config/relays-filename))
      (should (file-exists? @config/read-event-ids-filename))
      (should (file-exists? @config/tabs-filename))
      (prn (read-string (slurp @config/keys-filename)))
      )
    )

  (context "migration 2 - fix names"
    (it "fixes names"
      (with-redefs [rand-int (fn [_n] 12)]
        (let [bad-nicknames {1 "good-name"
                             2 "bad name"
                             3 "long-name0123456789"
                             4 ""
                             5 nil
                             6 "洛奇安"}]
          (spit @config/nicknames-filename bad-nicknames)
          (migration-2-fix-names)
          (let [nicknames (read-string (slurp @config/nicknames-filename))]
            (should= {1 "good-name"
                      2 "badname"
                      3 "long-name0123456789"
                      4 "dud-12"
                      5 "dud-12"
                      6 "dudx-12"}
                     nicknames))))))

  (context "migration 3"
    (it "adds messages directory and empty message-file"
      (migration-3-add-messages-directory)
      (should (file-exists? @config/messages-directory))
      (should (is-directory? @config/messages-directory))
      (should (file-exists? @config/messages-filename))
      (should= {} (read-string (slurp @config/messages-filename)))
      )
    )

  (context "migration 4"
    (it "Adds profiles file and copies nicknames into empty profiles."
      (let [nicknames {1 "bob"
                       2 "bill"
                       }]
        (spit @config/nicknames-filename nicknames))
      (migration-4-add-profiles-and-load-with-nicknames)
      (should (file-exists? @config/profiles-filename))
      (let [profiles (read-string (slurp @config/profiles-filename))]
        (should= {1 {:name "bob"}
                  2 {:name "bill"}} profiles))))

  (context "migration 5"
    (it "Removes the nicknames file."
      (spit @config/nicknames-filename {1 "user-1"})
      (migration-5-remove-nicknames)
      (should-not (file-exists? @config/nicknames-filename))))

  (context "migration 6 reformat tabs file into tabs-list file"
    (it "reformats the tabs file."
      (let [tabs-map {:tab1 {:selected [1] :blocked [2]}
                      :tab2 {:selected [3 4] :blocked [5 6]}}]
        (spit @config/tabs-filename tabs-map))
      (should-not (file-exists? @config/tabs-list-filename))
      (migration-6-reformat-tabs)
      (should (file-exists? @config/tabs-list-filename))
      (let [tabs-list (read-string (slurp @config/tabs-list-filename))]
        (should (vector? tabs-list))
        (should= #{{:name "tab1" :selected [1] :blocked [2]}
                   {:name "tab2" :selected [3 4] :blocked [5 6]}}
                 (set tabs-list))))

    (it "reformats an empty tabs file"
      (spit @config/tabs-filename {})
      (migration-6-reformat-tabs)
      (should= [] (read-string (slurp @config/tabs-list-filename)))
      (should-not (file-exists? @config/tabs-filename)))

    (it "creates an empty tabs-list if no tabs"
      (migration-6-reformat-tabs)
      (should= [] (read-string (slurp @config/tabs-list-filename)))
      )
    )

  (context "migration 7 break message-file into daily files"
    (it "breaks up message-file and deletes it"
      (.mkdir (io/file "tmp/messages"))
      (should (file-exists? @config/messages-directory))
      (let [messages {1 {:id 1 :created-at 0}
                      2 {:id 2 :created-at 86400}
                      }]
        (spit @config/messages-filename messages))
      (migration-7-break-messages-into-daily-files)
      (should (file-exists? (str @config/messages-directory "/" "0-01Jan70")))
      (should (file-exists? (str @config/messages-directory "/" "1-02Jan70")))
      (should= [{:id 1 :created-at 0}] (read-string (slurp (str @config/messages-directory "/0-01Jan70"))))
      (should= [{:id 2 :created-at 86400}] (read-string (slurp (str @config/messages-directory "/1-02Jan70"))))
      (should-not (file-exists? @config/messages-filename))))

  (context "migration 8 user configuration"
    (it "creates a valid user-configuration file"
      (migration-8-user-configuration)
      (should (file-exists? @config/user-configuration-filename))
      (should= (user-configuration/validate {})
               (read-string (slurp @config/user-configuration-filename))))
    )

  (context "migration 9 contact list"
    (it "creates an empty contact-list file"
      (spit @config/keys-filename {:public-key (util/hexify 1N)})
      (migration-9-contact-lists)
      (should (file-exists? @config/contact-lists-filename))
      (should (contains?
                (read-string (slurp @config/contact-lists-filename))
                1N))))

  (context "migration 10 XTDB database"
    (with db (in-memory/get-db))
    (before-all (config/set-db! :in-memory))
    (before (in-memory/clear-db @db))

    (it "does not load profiles if no profile file is found"
      (with-redefs [gateway/add-profile (stub :add-profile)]
        (migration-10-load-profiles)
        (should-not-have-invoked :add-profile)))

    (it "loads profiles"
      (spit @config/profiles-filename {1 {:name "user 1"}
                                       2 {:name "user 2"}})
      (migration-10-load-profiles)
      (should= {:name "user 1"} (gateway/get-profile @db 1))
      (should= {:name "user 2"} (gateway/get-profile @db 2))
      (should-not (file-exists? @config/profiles-filename))
      (should (file-exists? (str @config/profiles-filename ".migrated"))))

    (it "does not load contacts if no contacts file is found"
      (with-redefs [gateway/add-contacts (stub :add-contacts)]
        (migration-10-load-contacts)
        (should-not-have-invoked :add-contacts)))

    (it "loads contacts"
      (spit @config/contact-lists-filename
            {1 [{:pubkey 99 :petname "pet-99"}
                {:pubkey 98 :petname "pet-98"}]
             2 [{:pubkey 97 :petname "pet-97"}
                {:pubkey 96 :petname "pet-96"}]})
      (migration-10-load-contacts)
      (should= [{:pubkey 99 :petname "pet-99"}
                {:pubkey 98 :petname "pet-98"}]
               (gateway/get-contacts @db 1))
      (should= [{:pubkey 97 :petname "pet-97"}
                {:pubkey 96 :petname "pet-96"}]
               (gateway/get-contacts @db 2))
      (should-not (file-exists? @config/contact-lists-filename))
      (should (file-exists? (str @config/contact-lists-filename ".migrated"))))

    (it "does not load events if there are no event files"
      (with-redefs [gateway/add-event (stub :add-event)]
        (migration-10-load-events)
        (should-not-have-invoked :add-event)))

    (it "loads events from all matching event files"
      (let [dir @config/messages-directory
            f1 "/1-1jan23"
            f2 "/2-2jan23"
            path1 (str dir f1)
            path2 (str dir f2)
            renamed-dir (str @config/messages-directory ".migrated")]
        (.mkdir (io/file "tmp/messages"))
        (spit path1 [{:id 1 :content "c1"}
                     {:id 2 :content "c2"}])
        (spit path2 [{:id 3 :content "c3"}
                     {:id 4 :content "c4"}])
        (migration-10-load-events)
        (should= {:id 1 :content "c1"} (gateway/get-event @db 1))
        (should= {:id 2 :content "c2"} (gateway/get-event @db 2))
        (should= {:id 3 :content "c3"} (gateway/get-event @db 3))
        (should= {:id 4 :content "c4"} (gateway/get-event @db 4))
        (should-not (file-exists? @config/messages-directory))
        (should-not (file-exists? path1))
        (should-not (file-exists? path2))
        (should (file-exists? renamed-dir))
        (should (file-exists? (str renamed-dir f1 ".migrated")))
        (should (file-exists? (str renamed-dir f2 ".migrated")))
        (prn 'got-here)))
    )

  (context "migration 11 password protect private key"
    (with db (in-memory/get-db))
    (before-all (config/set-db! :in-memory))
    (before (in-memory/clear-db @db)
            (mem/clear-mem))

    (it "password protects the keys file"
      (let [bytes-private-key (util/make-private-key)
            private-key (util/hexify (util/bytes->num bytes-private-key))]
        (spit @config/keys-filename {:private-key private-key})
        (migration-11-password-for-private-key)
        (let [keys (read-string (slurp @config/keys-filename))
              encrypted-private-key (:private-key keys)
              decoded-private-key (util/xor-string "password" (bech32/address->str encrypted-private-key))]
          (should= private-key decoded-private-key)
          (should= "password" (bech32/address->str (:password keys))))))

    (context "data storage changes"
      (it "reads the keys with no password"
        (let [bytes-private-key (util/make-private-key)
              hex-private-key (util/hexify (util/bytes->num bytes-private-key))
              bytes-public-key (es/get-pub-key bytes-private-key)
              public-key (util/bytes->num bytes-public-key)
              hex-public-key (util/hexify public-key)]
          (spit @config/keys-filename {:private-key hex-private-key
                                       :public-key hex-public-key})
          (data-storage/read-keys)
          (should= hex-public-key (mem/get-mem [:keys :public-key]))
          (should= hex-private-key (mem/get-mem [:keys :private-key]))
          (should= nil (mem/get-mem [:keys :password]))
          (should= public-key (mem/get-mem :pubkey))))

      (it "reads the keys with a password"
        (let [bytes-private-key (util/make-private-key)
              hex-private-key (util/hexify (util/bytes->num bytes-private-key))
              bytes-public-key (es/get-pub-key bytes-private-key)
              public-key (util/bytes->num bytes-public-key)
              hex-public-key (util/hexify public-key)
              password (bech32/encode-str "pw" "password")
              encoded-private-key (->> hex-private-key
                                       (util/xor-string "password")
                                       (bech32/encode-str "encoded"))
              ]

          (spit @config/keys-filename {:private-key encoded-private-key
                                       :public-key hex-public-key
                                       :password password})
          (data-storage/read-keys)
          (should= hex-public-key (mem/get-mem [:keys :public-key]))
          (should= hex-private-key (mem/get-mem [:keys :private-key]))
          (should= "password" (mem/get-mem [:keys :password]))
          (should= public-key (mem/get-mem :pubkey))))

      (it "writes the keys with no password"
        (let [bytes-private-key (util/make-private-key)
              hex-private-key (util/hexify (util/bytes->num bytes-private-key))
              bytes-public-key (es/get-pub-key bytes-private-key)
              public-key (util/bytes->num bytes-public-key)
              hex-public-key (util/hexify public-key)]
          (data-storage/write-keys {:private-key hex-private-key
                                    :public-key hex-public-key})
          (should= {:private-key hex-private-key
                    :public-key hex-public-key
                    :password nil}
                   (read-string (slurp @config/keys-filename)))))

      (it "writes the keys with a password"
        (let [bytes-private-key (util/make-private-key)
              hex-private-key (util/hexify (util/bytes->num bytes-private-key))
              bytes-public-key (es/get-pub-key bytes-private-key)
              public-key (util/bytes->num bytes-public-key)
              hex-public-key (util/hexify public-key)
              encoded-private-key (->> hex-private-key
                                       (util/xor-string "password")
                                       (bech32/encode-str "encoded"))]
          (data-storage/write-keys {:private-key hex-private-key
                                    :public-key hex-public-key
                                    :password "password"})
          (should= {:private-key encoded-private-key
                    :public-key hex-public-key
                    :password (bech32/encode-str "pw" "password")}
                   (read-string (slurp @config/keys-filename)))))
      )
    )
  )
