(ns more-speech.user-configuration-spec
  (:require [speclj.core :refer :all]
            [more-speech.user-configuration :refer :all]
            [more-speech.ui.swing.ui-context :refer :all]))

(describe "user configuration"
  (with-stubs)
  (context "basic functions"
    (before (reset! ui-context {:event-context (atom {})}))
    (it "sets and gets a config"
      (set-config :test "test")
      (should= "test" (get-config :test)))

    (it "sets and gets a nested config"
      (set-config [:test :nest] "nest")
      (should= "nest" (get-config [:test :nest])))
    )

  (context "export user profile"
    (it "creates if empty"
      (let [uc (validate {})]
        (should (contains? uc :export-user-profile))
        (should= 7 (get-in uc [:export-user-profile :export-after-days]))
        (should= 0 (get-in uc [:export-user-profile :last-time-exported]))))

    (it "decides if it is time to export"
      (with-redefs [keys-last-modified (fn [] 0)]
        (let [lte (* 2 86400)
              uc {:export-user-profile {:export-after-days 10
                                        :last-time-exported lte}}
              event-context (atom {:user-configuration uc})]
          (reset! ui-context {:event-context event-context})
          (should (should-export-profile? (* 12 86400)))
          (should (should-export-profile? (* 13 86400)))
          (should-not (should-export-profile? (* 11 86400))))))

    (it "will export if keys file has changed"
      (with-redefs [keys-last-modified (fn [] (* 3 86400))]
        (let [lte (* 2 86400)
              uc {:export-user-profile {:export-after-days 10
                                        :last-time-exported lte}}
              event-context (atom {:user-configuration uc})]
          (reset! ui-context {:event-context event-context})
          (should (should-export-profile? (* 12 86400)))
          (should (should-export-profile? (* 13 86400)))
          (should (should-export-profile? (* 11 86400))))))

    (it "won't export if :export-after-days is :never"
      (let [lte (* 2 86400)
            uc {:export-user-profile {:export-after-days :never
                                      :last-time-exported lte}}
            event-context (atom {:user-configuration uc})]
        (reset! ui-context {:event-context event-context})
        (should-not (should-export-profile? (* 12 86400)))
        (should-not (should-export-profile? (* 13 86400)))
        (should-not (should-export-profile? (* 11 86400)))))

    (it "sets the last exported time"
      (let [lte (* 2 86400)
            uc {:export-user-profile {:export-after-days 10
                                      :last-time-exported lte}}
            event-context (atom {:user-configuration uc})]
        (reset! ui-context {:event-context event-context}))
      (set-last-time-profile-exported 99)
      (let [event-context (:event-context @ui-context)]
        (should= 99 (get-in @event-context
                            [:user-configuration :export-user-profile :last-time-exported])))
      )
    )

  (context "gather historical metadata"
    (it "creates the configuration parameters if not present."
      (let [uc (validate {})]
        (should (contains? uc :import-metadata))
        (should= 30 (get-in uc [:import-metadata :import-after-days]))
        (should= 0 (get-in uc [:import-metadata :last-time-imported]))))

    (it "decides if it is time to import"
      (let [lti (* 2 86400)
            uc {:import-metadata {:import-after-days 10
                                  :last-time-imported lti}}
            event-context (atom {:user-configuration uc})]
        (reset! ui-context {:event-context event-context})
        (should (should-import-metadata? (* 12 86400)))
        (should (should-import-metadata? (* 13 86400)))
        (should-not (should-import-metadata? (* 11 86400)))))

    (it "sets the last imported time"
      (let [lti (* 2 86400)
            uc {:import-metadata {:import-after-days 10
                                  :last-time-imported lti}}
            event-context (atom {:user-configuration uc})]
        (reset! ui-context {:event-context event-context}))
      (set-last-time-metadata-imported 99)

        (should= 99 (get-in (get-event-state)
                            [:user-configuration :import-metadata :last-time-imported]))
      )
    )
  )

