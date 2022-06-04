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

(describe "reformat article body to fit width."
  (it "should not wrap nothing"
    (should= "" (reformat-article "" 1)))

  (it "should not wrap something shorter than the width."
    (should= "xxx" (reformat-article "xxx" 10)))

  (it "should wrap something longer than the width."
    (should= "xx\nxx" (reformat-article "xxxx" 2)))

  (it "should repeatedly wrap long strings."
    (should= "xx\nxx\nxx" (reformat-article "xxxxxx" 2)))

  (it "should break spaces"
    (should= "x\nx" (reformat-article "x x" 1))
    (should= "x\nx" (reformat-article "x x" 2))
    (should= "xx\nxx" (reformat-article "xx xx" 4)))

  (it "should ignore existing single line breaks."
    (should= "x x" (reformat-article "x\nx" 5)))

  (it "should preserve existing double line breaks."
    (should= "x\n\nx" (reformat-article "x\n\nx" 7)))

  (it "should preserve leading spaces."
    (should= "x\n xxx" (reformat-article "x\n xxx" 6)))

  (it "should give priority to leading spaces"
    (should= "x\n x\n\nx" (reformat-article "x\n x\n\nx" 20)))

  (it "should not break this line. (bug-fix)"
    (should= "I found you!" (reformat-article "I found you!" 50)))
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
        (should= "#[1]" (replace-references event))))

    ))
