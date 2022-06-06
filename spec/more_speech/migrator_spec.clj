(ns more-speech.migrator-spec
  (:require [speclj.core :refer :all]
            [more-speech.migrator :refer :all]
            ))

(describe "The Migrator"
  (with-stubs)
  (before-all (reset! migration-filename "tmp/migration"))
  (after (when (.exists (clojure.java.io/file @migration-filename))
           (clojure.java.io/delete-file @migration-filename)))

  (it "returns zero if no migration file"
    (should= 0 (get-migration-level)))

  (it "reads the migration level"
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