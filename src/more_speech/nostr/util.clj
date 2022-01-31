(ns more-speech.nostr.util)

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
    (byte-array byte-vector)))

(defn hex-string->num
  "Converts a hex string to a BigInteger"
  ^BigInteger [hex-string]
  (bytes->num (hex-string->bytes hex-string))
  )

(defn num->hex-string
  "converts a BigInteger to a hex-string"
  [^BigInteger num]
  (bytes->hex-string (num->bytes 32 num)))
