(ns more-speech.content.article-spec
  (:require [speclj.core :refer :all]
            [more-speech.content.article :as a :refer :all]
            [clojure.spec.alpha :as s]))

(describe "Formatting Utilities"
  (context "Abbreviations"
    (it "abbreviates pubkeys"
      (should= "short" (abbreviate-key "short"))
      (should= "long pub..." (abbreviate-key "long pubkey")))

    (it "abbreviates authors"
      (should= "short" (abbreviate-author "short"))
      (should= "a very long author n..." (abbreviate-author "a very long author name")))
    )

  (it "abbreviates bodies"
    (should= "short" (abbreviate-body "short"))
    (let [abbreviated (abbreviate-body (apply str (repeat 200 "*")))]
      (should= "***" (.substring abbreviated 0 3))
      (should= "..." (.substring abbreviated (- (count abbreviated) 3)))))
  )

(defn- valid-markup-token? [token]
  (or
    (keyword? token)
    (string? token)
    (number? token)))

(describe "Formatting an article"
  (let [article {:group "comp.lang.c++"
                 :author "Bob"
                 :time 1642683327
                 :subject "Subject"
                 :body "My Message to you."
                 :thread-count 15}]
    (it "conforms to spec."
      (should (s/valid? ::a/article article)))

    (it "is properly formatted"
      (let [markup(markup-header article)]
        (should (vector? markup))
        (should (every? valid-markup-token? markup))))))

(describe "Formatting an author nickname."
  (let [author-tuple [0 "nickname"]]
    (it "conforms to spec."
      (should (s/valid? ::a/author-nickname-tuple author-tuple)))

    (it "is properly formatted."
      (should= [:bold
                "00000000..."
                :regular
                " - "
                "nickname"
                :new-line]
               (markup-author author-tuple)))))


