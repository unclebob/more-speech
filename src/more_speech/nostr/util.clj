(ns more-speech.nostr.util
  (:import (java.security MessageDigest)))

(defn num->bytes
  "Returns the byte-array representation of n.
  The array will have the specified length."
  [length n]
  (let [a (.toByteArray (biginteger n))
        l (count a)
        zeros (repeat (- length l) (byte 0))]
    (if (> l length)
      (byte-array (drop (- l length) (seq a)))
      (byte-array (concat zeros a)))))

(defn bytes->num
  "Returns a BigInteger from a byte-array."
  ^BigInteger [^bytes bytes]
  (BigInteger. 1 bytes))

(defn bytes->hex-string
  "Returns a string containing the hexadecimal
  representation of the byte-array. This is the
  inverse of hex-string->bytes."
  [byte-array]
  (let [byte-seq (for [i (range (alength byte-array))] (aget byte-array i))
        byte-strings (map #(apply str (take-last 2 (format "%02x" %))) byte-seq)]
    (apply str (apply concat byte-strings))))

(defn ^bytes hex-string->bytes
  "returns a byte-array containing the bytes described
  by the hex-string.  This is the inverse of bytes->hex-string."
  [hex-string]
    (let [byte-strings (map #(apply str %) (partition 2 hex-string))
          byte-vector (map #(Integer/parseInt % 16) byte-strings)]
      (byte-array byte-vector))
    )

(defn hex-string->num
  "returns BigInteger from a hex string"
  [hex-string]
  (-> hex-string hex-string->bytes bytes->num))

(defn num32->hex-string [n]
  "converts a number to a 32 byte hex-string"
  (->> n (num->bytes 32) bytes->hex-string))

(defn bytes=
  "compares two byte arrays for equality."
  [^bytes b1 ^bytes b2]
  (assert (= (alength b1) (alength b2)) "bytes= args not same size.")
  (loop [index 0]
    (if (= index (alength b1))
      true
      (if (= (aget b1 index) (aget b2 index))
        (recur (inc index))
        false)))
  )

(defn sha-256
  "Returns the sha256 hash of the message.
  Both the message and the hash are byte-arrays."
  ^bytes [^bytes message]
  (let [digest (MessageDigest/getInstance "SHA-256")]
    (.digest digest message)))

(defn xor-bytes [^bytes a ^bytes b]
  (assert (= (alength a) (alength b)) "byte-wise-xor: arguments not same size.")
  (let [result (byte-array (alength a))]
    (doseq [i (range (alength a))]
      (aset result i (byte (bit-xor (aget a i) (aget b i)))))
    result))
