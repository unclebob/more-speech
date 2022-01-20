(ns more-speech.article-spec
  (:require [speclj.core :refer :all]
            [more-speech.article :as a :refer :all]
            [clojure.spec.alpha :as s]))

(describe "formatting an article"
  (let [article {:group "comp.lang.c++"
                 :author "Bob"
                 :time 1642683327
                 :subject "Subject"
                 :body "My Message to you."
                 :thread-count 15}]
    (it "conforms to spec."
      (should (s/valid? ::a/article article)))

    (it "is properly formatted"
      (should= [
                :bold
                "* Bob"
                :regular
                " (15)"
                :bold
                :pos 40
                "Subject"
                :regular
                :pos 80
                "20 Jan 22 06:55:27 CST"
                :new-line
                :multi-line "My Message to you."
                :line
                :new-line]
               (markup-article article)))))
