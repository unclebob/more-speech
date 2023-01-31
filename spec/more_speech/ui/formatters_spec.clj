(ns more-speech.ui.formatters-spec
  (:require [speclj.core :refer :all]
            [more-speech.db.gateway :as gateway]
            [more-speech.db.in-memory :as in-memory]
            [more-speech.ui.formatters :refer :all]
            [more-speech.ui.formatter-util :refer :all]
            [more-speech.mem :refer :all]
            [more-speech.config :as config]))

(describe "Abbreviations."
  (it "abbreviates pubkeys"
    (should= "short" (abbreviate "short" 10))
    (should= "some lo..." (abbreviate "some long string." 10))))

(declare db)
(describe "format header"
  (with db (in-memory/get-db))
  (before-all (config/set-db! :in-memory))
  (before (in-memory/clear-db @db)
          (set-mem :pubkey 99))

  (it "formats an empty message"
    (let [event {:pubkey 16r1111111111111111111111111111111111111111111111111111111111111111
                 :created-at 1
                 :content ""
                 :tags []}
          timestamp (format-time (event :created-at))
          header (format-header event)]
      (should= (str "          (1111111...) " timestamp " \n") header)))

  (it "formats a simple message"
    (let [event {:pubkey 16r1111111111111111111111111111111111111111111111111111111111111111
                 :created-at 1
                 :content "the message"
                 :tags []}
          timestamp (format-time (event :created-at))
          header (format-header event)]
      (should= (str "          (1111111...) " timestamp " the message\n") header)))

  (it "formats a simple message with a user profile"
    (gateway/add-profile @db 1 {:name "user-1"})
    (let [event {:pubkey 1
                 :created-at 1
                 :content "the message"
                 :tags []}
          timestamp (format-time (event :created-at))
          header (format-header event)]
      (should= (str "              (user-1) " timestamp " the message\n") header)))

  (it "formats a long message with line ends."
    (let [event {:pubkey 16r1111111111111111111111111111111111111111111111111111111111111111
                 :created-at 1
                 :content (str "Four score and seven years ago\n"
                               "our fathers brought forth upon this continent\n"
                               "a new nation concieved in liberty and dedicated to\n"
                               "the proposition that all men are created equal.")
                 :tags []}
          timestamp (format-time (event :created-at))
          header (format-header event)]
      (should (.startsWith header
                           (str "          (1111111...) "
                                timestamp
                                " Four score and seven years ago~our")))
      (should (.endsWith header "...\n"))))

  (it "formats a message with a subject"
    (let [event {:pubkey 16r1111111111111111111111111111111111111111111111111111111111111111
                 :created-at 1
                 :content "the message"
                 :tags [[:subject "the subject"]]}
          timestamp (format-time (event :created-at))
          header (format-header event)]
      (should= (str "          (1111111...) " timestamp " the subject|the message\n") header)))
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
  (with db (in-memory/get-db))
  (before-all (config/set-db! :in-memory))
  (before (in-memory/clear-db @db))

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
        (should= "#[1]" (replace-references event))))))

(describe "format-reply"
  (with db (in-memory/get-db))
  (before-all (config/set-db! :in-memory))
  (before (in-memory/clear-db @db)
          (gateway/add-profile @db 1 {:name "user-1"})
          (gateway/add-profile @db 2 {:name "user-2"}))
  (it "formats a reply to an event"
    (let [created-at (make-date "07/05/2022")
          relays ["relay-1"]
          tags [["p" (hexify 1)]]
          event {:pubkey 1 :created-at created-at
                 :relays relays :tags tags :content "Hello #[0]."}]
      (should=
        ">From: (user-1) at 07/05/22 00:00:00 on relay-1\n>---------------\n>Hello @user-1."
        (format-reply event))))

  (it "formats a reply to a DM"
    (let [created-at (make-date "07/05/2022")
          relays ["relay-1"]
          tags [["p" (hexify 2)]]
          event {:pubkey 1 :created-at created-at :dm true
                 :relays relays :tags tags :content "Hello #[0]."}]
      (should=
        "D @user-1\n>From: (user-1) at 07/05/22 00:00:00 on relay-1\n>---------------\n>Hello @user-2."
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
    (should= "<a href=\"https://nostr.com\">https://nostr.com</a>" (linkify "https://nostr.com")))
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
    (should= '() (segment-text-url "")))
  (it "returns a single :text element if no url in content"
    (should= '([:text "no url"]) (segment-text-url "no url")))
  (it "returns a single :url element if whole content is a url"
    (should= '([:url "http://nostr.com"]) (segment-text-url "http://nostr.com")))
  (it "returns a list of :text and :url elements when content contains multiple text and url segments"
    (should= '([:text "Check this "] [:url "http://nostr.com"] [:text " It's cool"])
             (segment-text-url "Check this http://nostr.com It's cool")))
  )

(describe "Format article"
  (it "should escape HTML entities"
    (should= "&lt;b&gt;text&lt;&#x2F;b&gt;" (reformat-article "<b>text</b>")))
  (it "should linkify url"
    (should= "<a href=\"https://nostr.com\">https://nostr.com</a>" (reformat-article "https://nostr.com")))
  (it "should escape HTML entities and linkify url"
    (should= "&lt;b&gt;Clojure&lt;&#x2F;b&gt;: <a href=\"https://clojure.org/\">https://clojure.org/</a>"
             (reformat-article "<b>Clojure</b>: https://clojure.org/")))
  (it "should format replies and escape HTML entities properly"
    (should= "&gt;this is<br>&gt;a reply" (reformat-article ">this is >a reply")))
  (it "should replace multiple spaces with &nbsp"
    (should= "one two&nbsp three&nbsp&nbsp ." (reformat-article "one two  three   .")))
  )

(declare db)
(describe "Format User ID"
  (with db (in-memory/get-db))
  (before-all (config/set-db! :in-memory))
  (before (in-memory/clear-db @db)
          (clear-mem))
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