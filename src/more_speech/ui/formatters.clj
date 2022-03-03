(ns more-speech.ui.formatters
  (:import (java.util Date)
             (java.text SimpleDateFormat)))

(defn format-time [time]
  (let [time (* time 1000)
        date (Date. (long time))]
    (.format (SimpleDateFormat. "dd MMM yy kk:mm:ss z") date))
  )

(defn abbreviate [s n]
  (if (<= (count s) n)
    s
    (str (subs s 0 n) "...")))