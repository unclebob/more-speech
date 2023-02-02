(ns more-speech.util.files
  (:require [clojure.java.io :as io]))

(defn file-exists? [fname]
  (.exists (io/file fname)))

(defn is-directory? [fname]
  (.isDirectory (io/file fname)))

(defn delete-file [fname]
  (when (file-exists? fname)
    (io/delete-file fname)))

(defn rename-file [fname to-fname]
  (when (file-exists? fname)
    (.renameTo (io/file fname) (io/file to-fname))))

(defn delete-dir
  [& fs]
  (when-let [f (first fs)]
    (when (file-exists? f)
      (if-let [cs (seq (.listFiles (io/file f)))]
        (recur (concat cs fs))
        (do
          (io/delete-file f)
          (recur (rest fs)))))))