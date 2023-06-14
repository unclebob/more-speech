(ns more-speech.ui.formatters-spec
  (:require
    [more-speech.bech32 :as bech32]
    [more-speech.db.gateway :as gateway]
    [more-speech.mem :refer :all]
    [more-speech.nostr.util :refer [hexify]]
    [more-speech.spec-util :refer :all]
    [more-speech.ui.formatter-util :refer :all]
    [more-speech.ui.formatters :refer :all]
    [speclj.core :refer :all]))

(describe "Abbreviations."
  (it "abbreviates pubkeys"
    (should= "short" (abbreviate "short" 10))
    (should= "some lo..." (abbreviate "some long string." 10))))

(defn trim-header [header]
  (.trim (.replace header \ᐧ \ )))

(declare db)
(describe "format header"
  (setup-db-mem)
  (before (set-mem :pubkey 99))

  (it "formats an empty message"
    (let [event {:pubkey 16r1111111111111111111111111111111111111111111111111111111111111111
                 :created-at 1
                 :content ""
                 :tags []}
          timestamp (format-time (event :created-at))
          header (trim-header (format-header event))]
      (should= (str "(1111111...) " timestamp) header)))

  (it "formats a simple message"
    (let [event {:pubkey 16r1111111111111111111111111111111111111111111111111111111111111111
                 :created-at 1
                 :content "the message"
                 :tags []}
          timestamp (format-time (event :created-at))
          header (trim-header (format-header event))]
      (should= (str "(1111111...) " timestamp " the message") header)))

  (it "formats a simple message with a user profile"
    (gateway/add-profile @db 1 {:name "user-1"})
    (let [event {:pubkey 1
                 :created-at 1
                 :content "the message"
                 :tags []}
          timestamp (format-time (event :created-at))
          header (trim-header (format-header event))]
      (should= (str "(user-1) " timestamp " the message") header)))

  (it "formats a message with a user reference"
    (gateway/add-profile @db 1 {:name "user-1"})
    (gateway/add-profile @db 2 {:name "user-2"})
    (let [npub2 (bech32/encode "npub" 2)
          event {:pubkey 1
                 :created-at 1
                 :content (str "the message nostr:" npub2)
                 :tags []}
          timestamp (format-time (event :created-at))
          header (trim-header (format-header event))]
      (should= (str "(user-1) " timestamp " the message nostr:user-2") header)))

  (it "formats a message with a user reference that has a petname"
    (gateway/add-profile @db 1 {:name "user-1"})
    (gateway/add-profile @db 2 {:name "user-2"})
    (set-mem :pubkey 1)
    (gateway/add-contacts @db 1 [{:pubkey 2 :petname "petname"}])
    (let [npub2 (bech32/encode "npub" 2)
          event {:pubkey 1
                 :created-at 1
                 :content (str "the message nostr:" npub2)
                 :tags []}
          timestamp (format-time (event :created-at))
          header (trim-header (format-header event))]
      (should= (str "user-1 " timestamp " the message nostr:petname") header)))

  (it "formats a long message with line ends."
    (let [event {:pubkey 16r1111111111111111111111111111111111111111111111111111111111111111
                 :created-at 1
                 :content (str "Four score and seven years ago\n"
                               "our fathers brought forth upon this continent\n"
                               "a new nation concieved in liberty and dedicated to\n"
                               "the proposition that all men are created equal.")
                 :tags []}
          timestamp (format-time (event :created-at))
          header (trim-header (format-header event))]
      (should (.startsWith header
                           (str "(1111111...) "
                                timestamp
                                " Four score and seven years ago~our")))
      (should (.endsWith header "..."))))

  (it "formats a message with a subject"
    (let [event {:pubkey 16r1111111111111111111111111111111111111111111111111111111111111111
                 :created-at 1
                 :content "the message"
                 :tags [[:subject "the subject"]]}
          timestamp (format-time (event :created-at))
          header (trim-header (format-header event))]
      (should= (str "(1111111...) " timestamp " the subject|the message") header)))
  )

(describe "subject and discussion tags"
  (context "get-subject"
    (it "returns null if no tags"
      (let [tags []
            subject (get-subject tags)]
        (should= nil subject)))

    (it "returns null if no subject tag"
      (let [tags [[:p "hi"]]
            subject (get-subject tags)]
        (should= nil subject)))

    (it "returns subject if found"
      (let [tags [[:p "hi"] [:subject "the subject"]]
            subject (get-subject tags)]
        (should= "the subject" subject)))
    ))

(describe "Replacing References"
  (setup-db-mem)

  (context "using #[n] and p tags"
    (before (gateway/add-profile @db 0 {:name "x"}))

    (it "replaces nothing if nothing to replace"
      (let [content "content"
            event {:content content}]
        (should= "content" (replace-references event))))

    (it "replaces a single p reference"
      (let [content "the #[0] reference"
            event {:content content :tags [[:p (hexify 0)]]}]
        (should= "the @x reference" (replace-references event))))

    (it "replaces a single p reference at the start"
      (let [content "#[0] reference"
            event {:content content :tags [[:p (hexify 0)]]}]
        (should= "@x reference" (replace-references event))))

    (it "replaces a single p reference at the end"
      (let [content "the #[0]"
            event {:content content :tags [[:p (hexify 0)]]}]
        (should= "the @x" (replace-references event))))

    (it "replaces a single p reference alone"
      (let [content "#[0]"
            event {:content content :tags [[:p (hexify 0)]]}]
        (should= "@x" (replace-references event))))

    (it "replaces a two p references"
      (gateway/add-profile @db 1 {:name "y"})
      (let [content "the #[0] and #[1] reference"
            event {:content content :tags [[:p (hexify 0)]
                                           [:p (hexify 1)]]}]
        (should= "the @x and @y reference" (replace-references event))))

    (it "Replaces a p reference with an abbreviated id if not a profile name"
      (let [content "#[0]"
            event {:content content :tags [[:p "deadbeef"]]}]
        (should= "@00000000000000000000000000000000000000000000000000000000deadbeef"
                 (replace-references event))))

    (it "does not replace reference if there is no p tag"
      (let [content "#[1]"
            event {:content content :tags [[:p "deadbeef"]]}]
        (should= "#[1]" (replace-references event))))

    (it "does not replace nostr:<bech32> if no user is referenced "
      (let [content "nostr:npub1qq"
            event {:content content}]
        (should= "nostr:npub1qq" (replace-references event))))
    )
  )

(describe "format-reply"
  (setup-db-mem)
  (before (gateway/add-profile @db 1 {:name "user-1"})
          (gateway/add-profile @db 2 {:name "user-2"}))
  (it "formats a reply to an event"
    (let [created-at (make-date "07/05/2022")
          relays ["relay-1"]
          tags [[:p (hexify 1)]]
          event {:pubkey 1 :created-at created-at
                 :relays relays :tags tags :content "Hello #[0]."}]
      (should=
        "From: (user-1) at 07/05 00:00\n\n> Hello @user-1.\n\nCC: @user-1\n"
        (format-reply event))))

  (it "formats a reply to a DM"
    (let [created-at (make-date "07/05/2022")
          relays ["relay-1"]
          tags [[:p (hexify 2)]]
          event {:pubkey 1 :created-at created-at :dm true
                 :relays relays :tags tags :content "Hello #[0]."}]
      (should=
        "D @user-1\nFrom: (user-1) at 07/05 00:00\n\n> Hello @user-2.\n\nCC: @user-2\n"
        (format-reply event))))
  )

(describe "Escape HTML entities"
  (it "returns the same string in the absence of any HTML entities"
    (let [content "Hi from more-speech"
          escaped-content (html-escape content)]
      (should= "Hi from more-speech" escaped-content)))
  (it "escapes `&`"
    (let [content "bread & butter"
          escaped-content (html-escape content)]
      (should= "bread &amp; butter" escaped-content)))
  (it "escapes `<`"
    (let [content "< less than"
          escaped-content (html-escape content)]
      (should= "&lt; less than" escaped-content)))
  (it "escapes `>`"
    (let [content "> greater than"
          escaped-content (html-escape content)]
      (should= "&gt; greater than" escaped-content)))
  (it "escapes `\"`"
    (let [content "\"bread\""
          escaped-content (html-escape content)]
      (should= "&quot;bread&quot;" escaped-content)))
  (it "escapes `'`"
    (let [content "'bread'"
          escaped-content (html-escape content)]
      (should= "&#x27;bread&#x27;" escaped-content)))
  (it "escapes `/`"
    (let [content "/bread/"
          escaped-content (html-escape content)]
      (should= "&#x2F;bread&#x2F;" escaped-content))))

(describe "Linkify URL"
  (it "should wrap a hyperlink around the url string"
    (should= "<a href=\"https://nostr.com\">nostr.com</a>" (linkify "https://nostr.com")))
  )

(describe "Format replies"
  (it "always breaks at a reply prefix '>'"
    (should= ">this is\n>a reply." (format-replies ">this is >a reply.")))
  )

(describe "Newlines as <br>"
  (it "should replace newlines with br tag"
    (should= "xx<br>xx" (break-newlines "xx\nxx"))))

(describe "Breaking spaces"
  (it "should replace strings of spaces with non breaking spaces."
    (should= "just one" (non-breaking-spaces "just one"))
    (should= "two&nbsp spaces" (non-breaking-spaces "two  spaces"))
    (should= "three&nbsp&nbsp spaces" (non-breaking-spaces "three   spaces"))
    (should= "1 2&nbsp 3&nbsp&nbsp 4&nbsp&nbsp&nbsp ."
             (non-breaking-spaces "1 2  3   4    ."))
    ))

(describe "Segment article content"
  (it "returns empty list if content is empty"
    (should= '() (segment-article "")))

  (it "returns a single :text element if no url in content"
    (should= '([:text "no url"]) (segment-article "no url")))

  (it "returns a single :url element if whole content is a url"
    (should= '([:url "http://nostr.com"]) (segment-article "http://nostr.com")))

  (it "returns a :namereference segment"
    (should= [[:namereference "@name"] [:text " text"]]
             (segment-article "@name text")))

  (it "returns an :idreference segment"
    (should= [[:idreference "@0000000000000000000000000000000000000000000000000000000000000000"] [:text " text"]]
             (segment-article "@0000000000000000000000000000000000000000000000000000000000000000 text")))

  (it "returns a list of :text and :url and :namereference segments"
    (should= [[:text "Hey "] [:namereference "@bob"] [:text " Check this "] [:url "http://nostr.com"] [:text " It's cool"]]
             (segment-article "Hey @bob Check this http://nostr.com It's cool"))
    (should= [[:nostrnpubreference "nostr:npub1qq"]]
             (segment-article "nostr:npub1qq"))
    (should= [[:nostrnotereference "nostr:note1qq"]]
             (segment-article "nostr:note1qq")))

  (it "extracts text from segments"
    (should= "name" (extract-reference "@name"))
    (should= "npub1qq" (extract-reference "npub1qq"))
    (should= "x" (extract-reference "nostr:x")))
  )

(declare user-id npub)

(describe "Format article"
  (setup-db-mem)

  (it "should escape HTML entities"
    (should= "&lt;b&gt;text&lt;&#x2F;b&gt;" (reformat-article-into-html "<b>text</b>")))

  (it "should linkify url"
    (should= "<a href=\"https://nostr.com\">nostr.com</a>" (reformat-article-into-html "https://nostr.com")))

  (it "should ms-link a namereference"
    (should= "<a href=\"ms-namereference://name\">@name</a>"
             (reformat-article-into-html "@name")))

  (it "should ms-link an idreference"
    (should= "<a href=\"ms-idreference://0000000000000000000000000000000000000000000000000000000000000000\">@0000000000000000000000000000000000000000000000000000000000000000</a>"
             (reformat-article-into-html "@0000000000000000000000000000000000000000000000000000000000000000")))

  (it "should escape HTML entities and linkify url"
    (should= "&lt;b&gt;Clojure&lt;&#x2F;b&gt;: <a href=\"https://clojure.org/\">clojure.org/</a>"
             (reformat-article-into-html "<b>Clojure</b>: https://clojure.org/")))

  (it "should format replies and escape HTML entities properly"
    (should= "&gt;this is<br>&gt;a reply" (reformat-article-into-html ">this is >a reply")))

  (it "should replace multiple spaces with &nbsp"
    (should= "one two&nbsp three&nbsp&nbsp ." (reformat-article-into-html "one two  three   .")))

  (context "mentions"
    (it "should replace @name with namereference link"
      (should= "<a href=\"ms-namereference://name\">@name</a>"
               (reformat-article-into-html "@name")))

    (it "should replace nostr:name with namereference link"
      (should= "<a href=\"ms-namereference://name\">nostr:name</a>"
               (reformat-article-into-html "nostr:name")))

    (context "formatting bech32 references"
      (with user-id (rand-int 1000000000))
      (with npub (bech32/encode "npub" @user-id))

      (it "should replace nostr:npub with namereference link using user's name "
        (gateway/add-profile @db @user-id {:name "user1"})
        (should= "<a href=\"ms-namereference://user1\">nostr:user1</a>"
                 (reformat-article-into-html (str "nostr:" @npub))))

      (it "should replace @npub with namereference link using user's name "
        (gateway/add-profile @db @user-id {:name "user1"})
        (should= "<a href=\"ms-namereference://user1\">@user1</a>"
                 (reformat-article-into-html (str "@" @npub))))

      (it "should replace nostr:npub with namereference link using npub if user does not exist "
        (should= (str "<a href=\"ms-namereference://"
                      @npub "\">nostr:" @npub "</a>")
                 (reformat-article-into-html (str "nostr:" @npub))))


      ;(it "should replace nostr:nprofile with namereference link using user's name "
      ;  (let [user-id (rand-int 1000000000)
      ;        npub (bech32/encode "nprofile" user-id)]
      ;    (gateway/add-profile @db user-id {:name "user1"})
      ;    (should= "<a href=\"ms-namereference://user1\">nostr:user1</a>"
      ;             (reformat-article-into-html (str "nostr:" npub)))))

      (it "should replace nostr:nevent with notereference link"
        (let [nevent (bech32/tlv-encode "nevent" {:special (hexify @user-id)})]
          (should= (str "<a href=\"ms-neventreference://"
                        nevent "\">nostr:[event]</a>")
                   (reformat-article-into-html (str "nostr:" nevent)))))

      (it "should replace nostr:note with notereference link"
        (let [note (bech32/encode "note" @user-id)]
          (should= (str "<a href=\"ms-notereference://"
                        note "\">nostr:" note "</a>")
                   (reformat-article-into-html (str "nostr:" note)))))
      )
    )
  )

(declare db)
(describe "Format User ID"
  (setup-db-mem)

  (it "shows untrusted pubkey if no profile or petname"
    (let [pubkey 16rdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef]
      (should= "(deadbee...)" (format-user-id pubkey 30))))

  (it "shows untrusted profile name if no petname"
    (gateway/add-profile @db 1 {:name "the name"})
    (should= "(the name)" (format-user-id 1 30)))

  (it "shows trusted petname if present"
    (let [my-pubkey 99
          his-pubkey 1]
      (gateway/add-profile @db his-pubkey {:name "his name"})
      (gateway/add-contacts @db my-pubkey [{:pubkey his-pubkey :petname "pet name"}])
      (set-mem :pubkey my-pubkey)
      (should= "pet name" (format-user-id his-pubkey))))

  (it "shows trusted profile name if trusted, but not pet name"
    (let [my-pubkey 99
          his-pubkey 1]
      (gateway/add-profile @db his-pubkey {:name "his name"})
      (gateway/add-contacts @db my-pubkey [{:pubkey his-pubkey}])
      (set-mem :pubkey my-pubkey)
      (should= "his name" (format-user-id his-pubkey))))

  (it "shows second degree of trust for user trusted by trusted user"
    (let [my-pubkey 99
          trusted-user 1
          trusted-by-trusted-user 2]
      (gateway/add-profile @db trusted-user {:name "trusted"})
      (gateway/add-profile @db trusted-by-trusted-user {:name "2-deg"})
      (gateway/add-contacts @db my-pubkey [{:pubkey trusted-user}])
      (gateway/add-contacts @db trusted-user [{:pubkey trusted-by-trusted-user}])
      (set-mem :pubkey my-pubkey)
      (should= "2-deg<-trusted" (format-user-id trusted-by-trusted-user))))

  (it "shows second degree of trust petname for user trusted by trusted user"
    (let [my-pubkey 99
          trusted-user 1
          trusted-by-trusted-user 2]
      (gateway/add-profile @db trusted-user {:name "trusted"})
      (gateway/add-profile @db trusted-by-trusted-user {:name "2-deg"})
      (gateway/add-contacts @db my-pubkey [{:pubkey trusted-user :petname "trusted-pet"}])
      (gateway/add-contacts @db trusted-user [{:pubkey trusted-by-trusted-user}])
      (set-mem :pubkey my-pubkey)
      (should= "2-deg<-trusted-pet" (format-user-id trusted-by-trusted-user))))
  )

(describe "combine patterns"
  (it "combines a single pattern and name"
    (let [pattern (combine-patterns [:name1 #"pattern1"])]
      (should= java.util.regex.Pattern (type pattern))
      (should= "(?<name1>pattern1)" (str pattern))))

  (it "combines multiple patterns and names"
    (let [pattern (combine-patterns [:name1 #"pattern1"]
                                    [:name2 #"pattern2"])]
      (should= java.util.regex.Pattern (type pattern))
      (should= "(?<name1>pattern1)|(?<name2>pattern2)" (str pattern))))

  )
