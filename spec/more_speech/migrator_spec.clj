(ns more-speech.migrator-spec
  (:require [speclj.core :refer :all]
            [more-speech.migrator :refer :all]
            [more-speech.config :as config]
            [clojure.java.io :as io]))

(defn change-to-tmp-files []
  (reset! config/private-directory "tmp")
  (reset! config/migration-filename "tmp/migration")
  (reset! config/nicknames-filename "tmp/nicknames")
  (reset! config/keys-filename "tmp/keys")
  (reset! config/relays-filename "tmp/relays")
  (reset! config/read-event-ids-filename "tmp/read-event-ids")
  (reset! config/tabs-filename "tmp/tabs")
  (.mkdir (io/file "tmp"))
  )

(defn delete-all-tmp-files []
  (delete-file "tmp/migration")
  (delete-file "tmp/nicknames")
  (delete-file "tmp/keys")
  (delete-file "tmp/relays")
  (delete-file "tmp/read-event-ids")
  (delete-file "tmp/tabs")
  )


(describe "The Migrator"
  (with-stubs)
  (before-all (change-to-tmp-files))
  (after (delete-all-tmp-files))
  (after-all (delete-file "tmp"))

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
  )
