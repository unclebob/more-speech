(ns more-speech.article
  (:require [clojure.spec.alpha :as s])
  (:import (java.util Date)
           (java.text SimpleDateFormat)))

(s/def ::group string?)
(s/def ::subject string?)
(s/def ::author string?)
(s/def ::time number?)
(s/def ::body string?)
(s/def ::thread-count number?)
(s/def ::article (s/keys :req-un [::group ::subject ::author ::time ::body ::thread-count]))

(defn format-time [time]
  (let [time (* time 1000)
        date (Date. (long time))]
    (.format (SimpleDateFormat. "dd MMM yy kk:mm:ss z") date))
  )

(defn abbreviate-body [body]
  (if (< (count body) 100)
    body
    (str (subs body 0 100) "...")))

(defn abbreviate-author [author]
  (if (< (count author) 20)
    author
    (str (subs author 0 20) "...")
  ))

(defn markup-article [article]
  [
   :bold
   (str "* " (abbreviate-author (:author article)))
   :regular
   (str " (" (:thread-count article) ")")
   :bold
   :pos 40
   (:subject article)
   :regular
   :pos 80
   (format-time (:time article))
   :new-line
   :multi-line (abbreviate-body (:body article))
   :line
   :new-line])

(defn abbreviate-key [pubkey]
  (str (subs pubkey 0 8) "..."))

(defn markup-author [[pubkey name]]
  [:bold
   (abbreviate-key pubkey)
   :regular
   " - "
   name
   :new-line
   ])
