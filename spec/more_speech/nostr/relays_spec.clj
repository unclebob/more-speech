(ns more-speech.nostr.relays-spec
  (:require [speclj.core :refer :all]
            [more-speech.nostr.relays :refer :all :as relays]
            [clojure.spec.alpha :as s]))

(describe "Relays"
  (context "loads relays from the \"relays\" file and sets defaults."
    (it "loads a non-existent file"
      (load-relays nil)
      (should= @relays {})
      (should (s/conform ::relays/relays @relays)))

    (it "loads an empty file"
      (load-relays "")
      (should= @relays {})
      (should (s/conform ::relays/relays @relays)))

    (it "loads a relay file with one relay"
      (load-relays "{\"relay-url\" {:read true :write true}}")
      (should= @relays {"relay-url"
                        {:read true :write true :connection nil :subscribed false}})
      (should (s/conform ::relays/relays @relays)))

    (it "loads a relay file with more than one relay"
      (load-relays (str "{\"relay-url-1\" {:read true :write true}\n"
                        "\"relay-url-2\" {:read false :write false}}")
                   )
      (should= @relays {"relay-url-1"
                        {:read true :write true :connection nil :subscribed false}
                        "relay-url-2"
                        {:read false :write false :connection nil :subscribed false}})
      (should (s/conform ::relays/relays @relays))))

  )

