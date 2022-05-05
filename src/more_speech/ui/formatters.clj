(ns more-speech.ui.formatters
  (:require [clojure.string :as string]
            [more-speech.nostr.util :as util])
  (:import (java.util Date)
           (java.text SimpleDateFormat)))

(defn format-time [time]
  (let [time (* time 1000)
        date (Date. (long time))]
    (.format (SimpleDateFormat. "dd MMM yy kk:mm:ss z") date))
  )

(defn abbreviate [s n]
  (let [dots "..."]
    (if (<= (count s) n)
      s
      (str (subs s 0 (- n (count dots))) dots))))

(defn prepend> [text]
  (let [lines (string/split-lines text)
        lines (map #(str ">" %) lines)]
    (string/join "\n" lines)))

(defn reformat-article [article width]
  (let [blank-line (.lastIndexOf article "\n\n" width)
        indentation (.indexOf article "\n ")
        breakable-space (.lastIndexOf article " " width)
        [break-point break-string skip]
        (cond
          (< -1 indentation width) [indentation "\n " 2]
          (>= blank-line 0) [blank-line "\n\n" 2]
          (<= (count article) width) [(count article) "" 0]
          (>= breakable-space 0) [breakable-space "\n" 1]
          :else [width "\n" 0])]
    (let [head (.substring article 0 break-point)
          head (.replaceAll head "\n" " ")
          tail (.substring article (+ skip break-point))]
      (if (empty? tail)
        head
        (str head break-string (reformat-article tail width))))))

(defn format-user-id [nicknames user-id]
  (if (nil? user-id)
    ""
    (abbreviate (get nicknames user-id (util/num->hex-string user-id)) 20)))

(defn format-header [nicknames {:keys [pubkey created-at content] :as event}]
  (if (nil? event)
    "nil"
    (let [name (format-user-id nicknames pubkey)
          time (format-time created-at)
          content (string/replace content \newline \~)
          content (abbreviate content 50)]
      (format "%20s %s %s\n" name time content))))

(defn format-article [event-state {:keys [id pubkey created-at content]}]
  (let [nicknames (:nicknames event-state)
        time (format-time created-at)
        name (format-user-id nicknames pubkey)
        article (reformat-article content 80)
        formatted-id (abbreviate (util/num->hex-string id) 10)]
    (format "%s %20s %s\n%s" time name formatted-id article))
  )

(defn format-reply [event]
  (prepend> (reformat-article (:content event) 80)))