(ns more-speech.nostr.util
  (:import (java.awt Toolkit)
           (java.awt.datatransfer StringSelection)
           (java.security MessageDigest SecureRandom)))

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

(defn unhexify [hex-string]
  (bigint (hex-string->num hex-string)))

(defn num32->hex-string [n]
  "converts a number to a 32 byte hex-string"
  (->> n (num->bytes 32) bytes->hex-string))

(defn hexify [n]
  (num32->hex-string n))

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

(defn xor-string [pw s]
  (let [pwd (map int (cycle pw))
        sc (map int s)
        cipher-ints (map bit-xor pwd sc)]
    (apply str (map char cipher-ints))))

(defn xor-bytes [^bytes a ^bytes b]
  (assert (= (alength a) (alength b)) "byte-wise-xor: arguments not same size.")
  (let [result (byte-array (alength a))]
    (doseq [i (range (alength a))]
      (aset result i (byte (bit-xor (aget a i) (aget b i)))))
    result))

(defn make-private-key []
  (let [gen (SecureRandom.)
        key-bytes (byte-array 32)
        _ (.nextBytes gen key-bytes)]
    key-bytes))

(defn get-now-ms []
  (System/currentTimeMillis))

(defn get-now []
  (quot (get-now-ms) 1000))

(defn get-clipboard []
  (.getSystemClipboard (Toolkit/getDefaultToolkit)))

(defn copy-to-clipboard [text]
  (let [selection (StringSelection. text)]
    (.setContents (get-clipboard) selection selection)))

(defn process-tag [tag]
  (when (and (seq tag) (seq (first tag)))
    (let [tag-type (first tag)
          tag-type (if (clojure.string/blank? tag-type) "blank" tag-type)
          tag-args (rest tag)
          tag-type (.replace tag-type \: \-)]
      (concat [(keyword tag-type)] tag-args))))

(defn process-tags [tags]
  (remove nil? (map process-tag tags)))

(defn translate-event [event]
  (let [id (unhexify (get event "id"))
        pubkey (unhexify (get event "pubkey"))
        sig (unhexify (get event "sig"))]
    {:id id
     :pubkey pubkey
     :created-at (get event "created_at")
     :kind (get event "kind")
     :content (get event "content")
     :sig sig
     :tags (process-tags (get event "tags"))}))
