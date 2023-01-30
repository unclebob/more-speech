(ns more-speech.user-configuration-spec
  (:require [speclj.core :refer :all]
            [more-speech.user-configuration :refer :all]
            [more-speech.mem :refer :all]))

(declare lte)

(describe "user configuration"
  (with-stubs)
  (context "basic functions"
    (before (clear-mem))
    (it "sets and gets a config"
      (set-config :test "test")
      (should= "test" (get-config :test)))

    (it "sets and gets a nested config"
      (set-config [:test :nest] "nest")
      (should= "nest" (get-config [:test :nest])))
    )

  (context "export user profile"
    (with lte (* 2 86400))
    (it "creates if empty"
      (let [uc (validate {})]
        (should (contains? uc :export-user-profile))
        (should= 7 (get-in uc [:export-user-profile :export-after-days]))
        (should= 0 (get-in uc [:export-user-profile :last-time-exported]))))

    (it "decides if it is time to export"
      (with-redefs [keys-last-modified (fn [] 0)]
        (set-config [:export-user-profile :export-after-days] 10)
        (set-config [:export-user-profile :last-time-exported] @lte)
        (should (should-export-profile? (* 12 86400)))
        (should (should-export-profile? (* 13 86400)))
        (should-not (should-export-profile? (* 11 86400)))))

    (it "will export if keys file has changed"
      (with-redefs [keys-last-modified (fn [] (* 3 86400))]
        (set-config [:export-user-profile :export-after-days] 10)
        (set-config [:export-user-profile :last-time-exported] @lte)
        (should (should-export-profile? (* 12 86400)))
        (should (should-export-profile? (* 13 86400)))
        (should (should-export-profile? (* 11 86400)))))

    (it "won't export if :export-after-days is :never"
      (set-config [:export-user-profile :export-after-days] :never)
      (set-config [:export-user-profile :last-time-exported] @lte)
      (should-not (should-export-profile? (* 12 86400)))
      (should-not (should-export-profile? (* 13 86400)))
      (should-not (should-export-profile? (* 11 86400))))

    (it "sets the last exported time"
      (set-config [:export-user-profile :export-after-days] 10)
      (set-config [:export-user-profile :last-time-exported] @lte)
      (set-last-time-profile-exported 99)
      (should= 99 (get-config [:export-user-profile :last-time-exported])))
    )

  (context "gather historical metadata"
    (it "creates the configuration parameters if not present."
      (let [uc (validate {})]
        (should (contains? uc :import-metadata))
        (should= 30 (get-in uc [:import-metadata :import-after-days]))
        (should= 0 (get-in uc [:import-metadata :last-time-imported]))))

    (it "decides if it is time to import"
      (set-config [:import-metadata :import-after-days] 10)
      (set-config [:import-metadata :last-time-imported] (* 2 86400))
      (should (should-import-metadata? (* 12 86400)))
      (should (should-import-metadata? (* 13 86400)))
      (should-not (should-import-metadata? (* 11 86400))))

    (it "sets the last imported time"
      (set-config [:import-metadata :import-after-days] 10)
      (set-config [:import-metadata :last-time-imported] (* 2 86400))
      (set-last-time-metadata-imported 99)
      (should= 99 (get-config [:import-metadata :last-time-imported])))
    )
  )

