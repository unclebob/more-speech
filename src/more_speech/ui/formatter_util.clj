(ns more-speech.ui.formatter-util
  (:require [clojure.string :as string]
            [more-speech.config :as config])
  (:import (java.text SimpleDateFormat)
           (java.util Date)))

(defn format-time [time]
  (let [time (* time 1000)
        date (Date. (long time))]
    (.format (SimpleDateFormat. "MM/dd/yy HH:mm:ss") date)))

(defn format-short-time [time]
  (let [time (* time 1000)
        date (Date. (long time))]
    (.format (SimpleDateFormat. "MM/dd HH:mm") date)))

(defn make-date [date-string]
  (let [date (.parse (SimpleDateFormat. "MM/dd/yyyy") date-string)]
    (quot (.getTime date) 1000)))

(defn abbreviate [s n]
  (let [dots "..."]
    (if (<= (count s) n)
      s
      (str (subs s 0 (- n (count dots))) dots))))

(defn prepend> [text]
  (let [lines (string/split-lines text)
        lines (map #(str "> " %) lines)]
    (string/join "\n" lines)))

(defn wrap-and-trim [s n l]
  (cond
    (zero? l) ""

    (< (count s) n) s

    :else
    (let [point (.lastIndexOf s " " n)
          point (if (< point 1) n point)]
      (str (subs s 0 point)
           "\n"
           (wrap-and-trim (subs s point) n (dec l))))))

(defn escape-html [s] (string/escape s config/html-escapes))
