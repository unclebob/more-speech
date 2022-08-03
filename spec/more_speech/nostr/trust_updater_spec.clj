(ns more-speech.nostr.trust-updater-spec
  (:require [speclj.core :refer :all]
            [more-speech.nostr.trust-updater :refer :all]
            [more-speech.ui.swing.ui-context :refer :all]))

(describe "Setting Trust"
  (with-stubs)
  (it "establishes new trust with a petname."
    (let [my-pubkey 1
          contact-lists {1 [{:pubkey 2}]}
          event-state {:pubkey my-pubkey :contact-lists contact-lists}]
      (reset! ui-context {:event-context (atom event-state)}))
    (entrust 3 "the-petname")
    (let [my-contacts (get (:contact-lists @(:event-context @ui-context)) 1)]
      (should= (set [{:pubkey 2}
                     {:pubkey 3 :petname "the-petname"}])
               (set my-contacts)))
    )

  (it "restablishes trust with a new petname."
      (let [my-pubkey 1
            contact-lists {1 [{:pubkey 1}
                              {:pubkey 3 :petname "the-old-petname"}
                              {:pubkey 2}]}
            event-state {:pubkey my-pubkey :contact-lists contact-lists}]
        (reset! ui-context {:event-context (atom event-state)}))
      (entrust 3 "the-new-petname")
      (let [my-contacts (get (:contact-lists @(:event-context @ui-context)) 1)]
        (should= (set [{:pubkey 1}
                       {:pubkey 2}
                       {:pubkey 3 :petname "the-new-petname"}])
                 (set my-contacts)))
      )
  )
