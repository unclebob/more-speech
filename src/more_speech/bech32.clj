(ns more-speech.bech32)

(def CHARSET "qpzry9x8gf2tvdw0s3jn54khce6mua7l")

(defn to-n [char]
  (.indexOf CHARSET (str char)))

(defn to-char [n]
  (get CHARSET n))

(defn validate-address-length [address]
  (if (>= 90 (count address))
    address
    (throw (Exception. "bech32: address too long"))))

(defn validate-hrp [hrp]
  (cond
    (empty? hrp)
    (throw (Exception. "bech32: no hrp"))

    (every? #(<= 33 (int %) 126) hrp)
    hrp

    :else
    (throw (Exception. "bech32: hrp invalid char"))))

(defn validate-data [data]
  (if (some #(neg? (to-n %)) data)
    (throw (Exception. "bech32: invalid data character"))
    data))

(defn find-separator-char [address]
  (let [separator-pos (.lastIndexOf address "1")]
    (if (neg? separator-pos)
      (throw (Exception. "bech32: no separator character (1)"))
      separator-pos)))

(defn validate-cksum [cksum]
  (cond
    (< (count cksum) 6)
    (throw (Exception. "bech32: checksum too short"))

    (some #(neg? (to-n %)) cksum)
    (throw (Exception. "bech32: invalid checksum character"))

    :else
    cksum))

(defn parse-address
  ([address]
   (parse-address address #{}))

  ([address options]
   (let [address (.toLowerCase address)
         _ (when-not (contains? options :no-length-restriction)
             (validate-address-length address))
         hrp-end (find-separator-char address)
         hrp (validate-hrp (subs address 0 hrp-end))
         data-and-cksum (subs address (inc hrp-end))
         cksum (apply str (take-last 6 data-and-cksum))
         cksum (validate-cksum cksum)
         data (apply str (drop-last 6 data-and-cksum))
         data (validate-data data)]
     [hrp data cksum])))

(def generator [0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3])

(defn polymod [values]
  (loop [values values
         chk 1]
    (if (empty? values)
      chk
      (let [value (first values)
            top (bit-shift-right chk 25)
            chk (-> chk
                    (bit-and 0x1ffffff)
                    (bit-shift-left 5)
                    (bit-xor value))
            chk (reduce
                  (fn [chk i]
                    (bit-xor
                      chk
                      (if (odd? (bit-shift-right top i))
                        (nth generator i)
                        0)))
                  chk (range 5))]
        (recur (rest values) chk)))))

(defn hrp-expand [hrp]
  (let [ns (map int hrp)
        hi (map #(bit-shift-right % 5) ns)
        lo (map #(bit-and % 31) ns)]
    (concat hi [0] lo)))

(defn verify-checksum? [[hrp data cksum]]
  (let [cksum-values (map to-n cksum)
        data-values (map to-n data)
        hrp-values (hrp-expand hrp)
        result (polymod (concat hrp-values data-values cksum-values))]
    (= 1 result)))

(defn create-checksum [hrp data]
  (let [hrp-data (hrp-expand hrp)
        pmod (polymod (concat hrp-data data [0 0 0 0 0 0]))
        pmod (bit-xor pmod 1)
        cksum-data (map #(-> pmod
                             (bit-shift-right (* 5 (- 5 %)))
                             (bit-and 31))
                        (range 6))]
    cksum-data))

(defn trim-leading-zeros [data]
  (if (= (repeat 8 0) (take 8 data))
    (recur (drop 8 data))
    data))

(defn encode
  "create bech32 address assume id is 64 byte integer"
  [hrp id]
  (let [byte-correction 16
        data (loop [id (* byte-correction id)
                    data []
                    chars 52]
               (if (zero? chars)
                 (trim-leading-zeros (reverse data))
                 (recur (quot id 32) (conj data (int (rem id 32))) (dec chars))))
        cksum-data (create-checksum hrp data)
        ]
    (str hrp "1" (apply str (map to-char (concat data cksum-data))))))

; The bech32 coding converts streams of bytes into streams of 5-bit integers.
; The numbers 5 and 8 are mutually prime so there will usually be a remainder.
; That remainder is the number of zero bits on the end of the encoded stream.
; Thus, a single byte abcdefgh gets encoded as two five bit integer
; as abcde fgh00.

(defn- align-num [n the-string]
  (let [n-bits (* 8 (count the-string))
        r (rem n-bits 5)]
    (if (zero? r)
      n
      (let [shift-n (- 5 r)
            shift-factor (reduce * (repeat shift-n 2))]
        (* n shift-factor)))))

(defn- str->num [the-string]
  (loop [s the-string
         n 0N]
    (if (empty? s)
      (align-num n the-string)
      (recur (rest s) (+ (* 256 n) (int (first s)))))))

(defn- num->32bit-data [n digits]
  (loop [n n
         data (list)
         digits digits]
    (if (zero? digits)
      data
      (recur (quot n 32)
             (conj data (int (rem n 32)))
             (dec digits)))))

(defn encode-str
  "create a bech32 representation of a string"
  [hrp s]
  (let [n-bits (* 8 (count s))
        n-digits (quot n-bits 5)
        n-digits (if (zero? (rem n-bits 5)) n-digits (inc n-digits))
        big-number (str->num s)
        data (num->32bit-data big-number n-digits)
        cksum-data (create-checksum hrp data)]
    (str hrp "1" (apply str (map to-char (concat data cksum-data))))))

(defn address->str [address]
  (let [[hrp data cksum] (parse-address address #{:no-length-restriction})
        valid? (verify-checksum? [hrp data cksum])]
    (if valid?
      (let [values (map to-n data)
            accumulator (reduce (fn [n value]
                                  (+ value (* n 32)))
                                0N values)
            n-bits (* 5 (count data))
            shift-n (rem n-bits 8)
            n-bytes (quot n-bits 8)
            aligned-accumulator (quot accumulator (reduce * (repeat shift-n 2)))]
        (loop [n aligned-accumulator
               chars (list)
               n-bytes n-bytes]
          (if (zero? n-bytes)
            (apply str chars)
            (recur (quot n 256)
                   (conj chars (char (rem n 256)))
                   (dec n-bytes)))))
      (throw (Exception. "bech32/address->str: invalid checksum")))
    )
  )

(defn address->number [address]
  (let [[hrp data cksum] (parse-address address)
        valid? (verify-checksum? [hrp data cksum])
        data-bits (* 5 (count data))
        byte-over (rem data-bits 8)
        byte-over-correction (reduce * (repeat byte-over 2))]
    (if valid?
      (let [values (map to-n data)
            acc (reduce (fn [n value]
                          (+ value (* n 32)))
                        0N values)]
        (/ acc byte-over-correction))
      (throw (Exception. "bech32: invalid checksum")))))



(defn address->tlv [address]
  (loop [chars (address->str address)
         tlv {}]
    (if (empty? chars)
      tlv
      (let [[t l & data] chars
            type (int t)
            length (int l)
            argument (take length data)
            remainder (drop length data)]
        (condp = type
          0 (let [hexid (apply str (map #(format "%02x" (int %)) argument))]
              (recur remainder (assoc tlv :special hexid)))

          1 (let [relay (apply str argument)
                  relays (:relays tlv)]
              (recur remainder (assoc tlv :relays (concat relays [relay]))))

          (recur remainder tlv))))))

(defn tlv-encode [hrp data]
  (let [id (:special data)
        relays (:relays data)
        id-bytes (map #(Integer/parseInt (apply str %) 16) (partition 2 id))
        tlv (concat [0 32] id-bytes)]
    (loop [relays relays
           tlv tlv]
      (if (empty? relays)
        (encode-str hrp tlv)
        (let [relay (first relays)
              relay-tlv [1 (count relay)]]
          (recur (rest relays) (concat tlv relay-tlv (map int relay))))))))
