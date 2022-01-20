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

(defn markup-article [article]
  [
   :bold
   (str "* " (:author article))
   :regular
   (str " (" (:thread-count article) ")")
   :bold
   :pos 40
   (:subject article)
   :regular
   :pos 80
   (format-time (:time article))
   :new-line
   :multi-line (:body article)
   :line
   :new-line])
