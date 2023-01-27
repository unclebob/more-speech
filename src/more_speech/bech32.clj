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

(defn parse-address [address]
  (let [address (validate-address-length (.toLowerCase address))
        hrp-end (find-separator-char address)
        hrp (validate-hrp (subs address 0 hrp-end))
        data-and-cksum (subs address (inc hrp-end))
        cksum (apply str (take-last 6 data-and-cksum))
        cksum (validate-cksum cksum)
        data (apply str (drop-last 6 data-and-cksum))
        data (validate-data data)]
    [hrp data cksum]))

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
