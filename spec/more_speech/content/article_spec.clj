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

(describe "threading of events"
  (it "does not thread if open events that have no references"
    (let [event1 {:id 1}
          event2 {:id 2}
          event-map {1 event1
                     2 event2}
          events [1 2]
          open-events #{1 2}
          threaded-events (thread-events events event-map open-events)]
      (should= [1 2] (map :id threaded-events))
      (should= [0 0] (map :indent threaded-events)))
    )

  (it "does not thread events that have references but are not open"
    (let [event1 {:id 1 :references [3]}
          event2 {:id 2}
          event3 {:id 3}
          event-map {1 event1
                     2 event2
                     3 event3}
          events [1 2 3]
          open-events #{}
          threaded-events (thread-events events event-map open-events)]
      (should= [1 2 3] (map :id threaded-events))
      (should= [0 0 0] (map :indent threaded-events))))

  (it "threads events that have references and are open"
    (let [event1 {:id 1 :references [3]}
          event2 {:id 2}
          event3 {:id 3}
          event-map {1 event1
                     2 event2
                     3 event3}
          events [1 2 3]
          open-events #{1}
          threaded-events (thread-events events event-map open-events)]
      (should= [1 3 2] (map :id threaded-events))
      (should= [0 1 0] (map :indent threaded-events))))

  (it "treats all articles threaded below an open article as open"
    (let [event1 {:id 1 :references [2]}
          event2 {:id 2 :references [3]}
          event3 {:id 3}
          event-map {1 event1
                     2 event2
                     3 event3}
          events [1 2 3]
          open-events #{1}
          threaded-events (thread-events events event-map open-events)]
      (should= [1 2 3] (map :id threaded-events))
      (should= [0 1 2] (map :indent threaded-events))))
  )


