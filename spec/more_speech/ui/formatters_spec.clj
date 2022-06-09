(ns more-speech.ui.formatters-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.formatters :refer :all]
            [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.nostr.util :as util]))

(defn hexify [n] (util/num32->hex-string n))

(describe "Abbreviations."
  (it "abbreviates pubkeys"
    (should= "short" (abbreviate "short" 10))
    (should= "some lo..." (abbreviate "some long string." 10)))

  )

(describe "format header"
  (it "formats an empty message"
    (let [nicknames {}
          event-context (atom {:nicknames nicknames})
          _ (reset! ui-context {:event-context event-context})
          event {:pubkey 16r1111111111111111111111111111111111111111111111111111111111111111
                 :created-at 1
                 :content ""
                 :tags []}
          timestamp (format-time (event :created-at))
          header (format-header nicknames event)]
      (should= (str "  11111111111111111... " timestamp " \n") header)))

  (it "formats a simple message"
    (let [nicknames {}
          event-context (atom {:nicknames nicknames})
          _ (reset! ui-context {:event-context event-context})
          event {:pubkey 16r1111111111111111111111111111111111111111111111111111111111111111
                 :created-at 1
                 :content "the message"
                 :tags []}
          timestamp (format-time (event :created-at))
          header (format-header nicknames event)]
      (should= (str "  11111111111111111... " timestamp " the message\n") header)))

  (it "formats a long message with line ends."
    (let [nicknames {}
          event-context (atom {:nicknames nicknames})
          _ (reset! ui-context {:event-context event-context})
          event {:pubkey 16r1111111111111111111111111111111111111111111111111111111111111111
                 :created-at 1
                 :content "Four score and seven years ago
our fathers brought forth upon this continent
a new nation concieved in liberty and dedicated to
the proposition that all men are created equal."
                 :tags []}
          timestamp (format-time (event :created-at))
          header (format-header nicknames event)]
      (should= (str "  11111111111111111... " timestamp " Four score and seven years ago~our fathers brought forth upon this continent~...\n") header)))

  (it "formats a message with a subject"
    (let [nicknames {}
          event-context (atom {:nicknames nicknames})
          _ (reset! ui-context {:event-context event-context})
          event {:pubkey 16r1111111111111111111111111111111111111111111111111111111111111111
                 :created-at 1
                 :content "the message"
                 :tags [[:subject "the subject"]]}
          timestamp (format-time (event :created-at))
          header (format-header nicknames event)]
      (should= (str "  11111111111111111... " timestamp " the subject|the message\n") header)))
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
  (context "using #[n] and p tags"
    (it "replaces nothing if nothing to replace"
      (let [nicknames {0 "x"}
            event-context (atom {:nicknames nicknames})
            _ (reset! ui-context {:event-context event-context})
            content "content"
            event {:content content}]
        (should= "content" (replace-references event))))

    (it "replaces a single p reference"
      (let [content "the #[0] reference"
            nicknames {0 "x"}
            event-context (atom {:nicknames nicknames})
            _ (reset! ui-context {:event-context event-context})
            event {:content content :tags [[:p (hexify 0)]]}]
        (should= "the @x reference" (replace-references event))))

    (it "replaces a single p reference at the start"
      (let [content "#[0] reference"
            nicknames {0 "x"}
            event-context (atom {:nicknames nicknames})
            _ (reset! ui-context {:event-context event-context})
            event {:content content :tags [[:p (hexify 0)]]}]
        (should= "@x reference" (replace-references event))))

    (it "replaces a single p reference at the end"
      (let [content "the #[0]"
            nicknames {0 "x"}
            event-context (atom {:nicknames nicknames})
            _ (reset! ui-context {:event-context event-context})
            event {:content content :tags [[:p (hexify 0)]]}]
        (should= "the @x" (replace-references event))))

    (it "replaces a single p reference alone"
      (let [content "#[0]"
            nicknames {0 "x"}
            event-context (atom {:nicknames nicknames})
            _ (reset! ui-context {:event-context event-context})
            event {:content content :tags [[:p (hexify 0)]]}]
        (should= "@x" (replace-references event))))

    (it "replaces a two p references"
      (let [content "the #[0] and #[1] reference"
            nicknames {0 "x"
                       1 "y"}
            event-context (atom {:nicknames nicknames})
            _ (reset! ui-context {:event-context event-context})
            event {:content content :tags [[:p (hexify 0)]
                                           [:p (hexify 1)]]}]
        (should= "the @x and @y reference" (replace-references event))))

    (it "Replaces a p reference with an abbreviated id if not a nickname"
      (let [content "#[0]"
            nicknames {0 "x"}
            event-context (atom {:nicknames nicknames})
            _ (reset! ui-context {:event-context event-context})
            event {:content content :tags [[:p "deadbeef"]]}]
        (should= "@id:deadbeef" (replace-references event))))

    (it "does not replace reference if there is no p tag"
      (let [content "#[1]"
            nicknames {0 "x"}
            event-context (atom {:nicknames nicknames})
            _ (reset! ui-context {:event-context event-context})
            event {:content content :tags [[:p "deadbeef"]]}]
        (should= "#[1]" (replace-references event))))))


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

(describe "Segment article content"
  (it "returns empty list if content is empty"
      (should= '() (segment-text-url "")))
  (it "returns a single :text element if no url in content"
      (should= '([:text "no url"]) (segment-text-url "no url")))
  (it "returns a single :url element if whole content is a url"
      (should= '([:url "http://nostr.com"]) (segment-text-url "http://nostr.com")))
  (it "returns a list of :text and :url elements when content contains multiple text and url segments"
      (should= '([:text "Check this "][:url "http://nostr.com"][:text " It's cool"])
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
  )