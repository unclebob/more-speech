(ns more-speech.ui.formatters
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