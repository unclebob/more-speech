(ns more-speech.nostr.relays-spec
  (:require [clojure.spec.alpha :as s]
            [more-speech.mem :refer :all]
            [more-speech.nostr.relays :refer :all :as relays]
            [speclj.core :refer :all]))

(describe "Relays"
  (context "loads relays from the \"relays\" file and sets defaults."
    (it "loads a non-existent file"
      (set-mem :relays {})
      (load-relays nil)
      (should= {} (get-mem :relays))
      (should (s/conform ::relays/relays (get-mem :relays))))

    (it "loads an empty file"
      (load-relays "")
      (should= (get-mem :relays) {})
      (should (s/conform ::relays/relays (get-mem :relays))))

    (it "loads a relay file with one relay"
      (load-relays "{\"relay-url\" {:read true :write true}}")
      (should= (get-mem :relays)
               {"relay-url"
                {:read true :write true :connection nil :subscribed false}})
      (should (s/conform ::relays/relays (get-mem :relays))))

    (it "loads a relay file with more than one relay"
      (load-relays (str "{\"relay-url-1\" {:read true :write true}\n"
                        "\"relay-url-2\" {:read false :write false}}")
                   )

      (should= {"relay-url-1"
                {:read true :write true :connection nil :subscribed false}
                "relay-url-2"
                {:read false :write false :connection nil :subscribed false}}
               (get-mem :relays))
      (should (s/conform ::relays/relays (get-mem :relays)))))
  )

(describe "relays-for-writing"
  (it "trims off the dynamic components and translates read from boolean"
    (set-mem :relays {"url1" {:read true :write true :junk "junk"}
                      "url2" {:read false :write false :junk "junk2"}
                      "url3" {:read :read-trusted :write false}})
    (should= {"url1" {:read :read-all :write true}
              "url2" {:read :read-none :write false}
              "url3" {:read :read-trusted :write false}}
             (relays-for-writing))))

(describe "relay-urls"
  (it "allows valid urls"
    (let [urls ["ws://hi.com"
                "wss://hi.com"
                "wss://expensive-relay.fiatjaf.com"
                "wss://freedom-relay.herokuapp.com"]]
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

  (it "changes removes dangling / from valid urls"
    (should= "wss://hi.com" (validate-relay-url "wss://hi.com/")))

  (it "trims and lower cases urls"
    (should= "wss://hi.com" (validate-relay-url " wss://HI.com\t")))

  (it "gets domain name of relay"
    (should= "hi.com" (get-domain-name "wss://hi.com"))
    (should= "filter.nostr.wine"
             (get-domain-name "wss://filter.nostr.wine/npub19mun7qwdyjf7qs3456u8kyxncjn5u2n7klpu4utgy68k4aenzj6synjnft?broadcast=true"))
    )
  )


