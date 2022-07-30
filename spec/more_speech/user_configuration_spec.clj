(ns more-speech.user-configuration-spec
  (:require [speclj.core :refer :all]
            [more-speech.user-configuration :refer :all]
            [more-speech.ui.swing.ui-context :refer :all]))

(describe "user configuration"
  (context "export user profile"
    (it "creates if empty"
      (let [uc (validate {})]
        (should (contains? uc :export-user-profile))
        (should= 7 (get-in uc [:export-user-profile :export-after-days]))
        (should= 0 (get-in uc [:export-user-profile :last-time-exported]))))

    (it "decides if it is time to export"
      (let [lte (* 2 86400)
            uc {:export-user-profile {:export-after-days 10
                                      :last-time-exported lte}}
            event-context (atom {:user-configuration uc})]
        (reset! ui-context {:event-context event-context})
        (should (should-export? (* 12 86400)))
        (should (should-export? (* 13 86400)))
        (should-not (should-export? (* 11 86400)))))

    (it "sets the last exported time"
      (let [lte (* 2 86400)
            uc {:export-user-profile {:export-after-days 10
                                      :last-time-exported lte}}
            event-context (atom {:user-configuration uc})]
        (reset! ui-context {:event-context event-context}))
      (set-last-time-exported 99)
      (let [event-context (:event-context @ui-context)]
        (should= 99 (get-in @event-context
                            [:user-configuration :export-user-profile :last-time-exported])))
      )
    )
  )
