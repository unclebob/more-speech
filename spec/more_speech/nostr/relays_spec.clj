(ns more-speech.nostr.relays-spec
  (:require [speclj.core :refer :all]
            [more-speech.mem :refer :all]
            [more-speech.nostr.relays :refer :all :as relays]
            [clojure.spec.alpha :as s]))

(describe "Relays"
  (context "loads relays from the \"relays\" file and sets defaults."
    (it "loads a non-existent file"
      (reset! relays {})
      (load-relays nil)
      (should= {} @relays)
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

      (should= {"relay-url-1"
                {:read true :write true :connection nil :subscribed false}
                "relay-url-2"
                {:read false :write false :connection nil :subscribed false}}
               @relays)
      (should (s/conform ::relays/relays @relays))))
  )

(describe "relays-for-writing"
  (it "trims off the dynamic components"
    (reset! relays {"url1" {:read true :write true :junk "junk"}
                    "url2" {:read false :write false :junk "junk2"}})
    (should= {"url1" {:read true :write true}
              "url2" {:read false :write false}}
             (relays-for-writing))))

(describe "validate-relay-url"
  (it "allows valid urls"
    (let [urls ["ws://hi.com"
                "wss://hi.com"
                "wss://expensive-relay.fiatjaf.com"
                "ws://hi.com:99"
                "wss://freedom-relay.herokuapp.com/ws"
                "ws://nostr-pub.wellorder.net:7000"]]
      (doseq [url urls]
        (should= url (validate-relay-url url))))
    )

  (it "does not allow invalid urls"
    (let [urls ["junk"
                "http://hi.com"
                "https://hi.com"
                "ws://localhost:70"]]
      (doseq [url urls]
        (should-be-nil (validate-relay-url url)))))

  (it "trims and lower cases urls"
    (should= "wss://hi.com" (validate-relay-url " wss://HI.com\t")))

  )


