(ns more-speech.content.article
  (:require [clojure.spec.alpha :as s]
            [more-speech.nostr.util :refer [num->hex-string]])
  (:import (java.util Date)
           (java.text SimpleDateFormat)))

(s/def ::group string?)
(s/def ::subject string?)
(s/def ::author string?)
(s/def ::time number?)
(s/def ::body string?)
(s/def ::thread-count number?)
(s/def ::article (s/keys :req-un [::group ::subject ::author ::time ::body ::thread-count]))

(s/def ::author-nickname string?)
(s/def ::author-pubkey string?)
(s/def ::author-nickname-tuple (s/tuple ::author-pubkey ::author-nickname))

(defn make-article [name time body thread-count indent]
  {:group ""
   :author name
   :subject "?"
   :time time
   :body body
   :thread-count thread-count
   :indent indent}
  )

(defn format-time [time]
  (let [time (* time 1000)
        date (Date. (long time))]
    (.format (SimpleDateFormat. "dd MMM yy kk:mm:ss z") date))
  )

(defn abbreviate [s n]
  (if (<= (count s) n)
    s
    (str (subs s 0 n) "...")))

(defn abbreviate-body [body]
  (abbreviate body 100))

(defn abbreviate-author [author]
  (abbreviate author 20))

(defn abbreviate-key [pubkey]
  (abbreviate pubkey 8))

(defn markup-article [article]
  (let [thread-count (:thread-count article)
        indent (get article :indent 0)]
    [
     :regular
     (apply str (repeat indent "•"))
     (if (> thread-count 0)
       :open-button
       :null-button)
     :bold
     (abbreviate-author (:author article))
     :regular
     (str " (" thread-count ")")
     :bold
     :pos 40
     (:subject article)
     :regular
     :pos 80
     (format-time (:time article))
     :new-line
     :multi-line (abbreviate-body (:body article))
     :line
     :new-line]))

(defn markup-author [[pubkey name]]
  [:bold
   (abbreviate-key (num->hex-string pubkey))
   :regular
   " - "
   name
   :new-line
   ])
