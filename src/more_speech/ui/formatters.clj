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
  (let [double-break (.lastIndexOf article "\n\n" width)
        break-space (.indexOf article "\n ")]
    (cond
      (>= double-break 0)
      (let [line (.substring article 0 double-break)
            line (.replaceAll line "\n" " ")]
        (str line "\n\n"
             (reformat-article
               (.substring article (+ 2 double-break))
               width)))

      (< -1 break-space width)
      (let [line (.substring article 0 break-space)
            line (.replaceAll line "\n" " ")]
        (str line "\n"
             (reformat-article
               (.substring article (inc break-space))
               width)))

      (<= (count article) width)
      (.replaceAll article "\n" " ")

      :else
      (let [break-position (.lastIndexOf article " " width)
            break-position (if (neg-int? break-position)
                             width
                             break-position)
            head (.substring article 0 break-position)
            head (.replaceAll head "\n" " ")
            tail (.substring article break-position)]
        (str
          (.trim head)
          "\n"
          (reformat-article (.trim tail) width)))
      )))