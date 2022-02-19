(ns more-speech.ui.app-util
  (:require [more-speech.ui.widget :as w]))

(def next-update-path [:application :next-update])
(def this-update-path [:application :this-update])
(def mouse-lock-path [:application :mouse-locked-to])

(defn update-widget [state widget]
  (cond
    (satisfies? w/widget widget)
    (update-in state next-update-path conj (:path widget))

    (coll? widget)
    (update-in state next-update-path conj widget)

    :else
    (assert false "Bad widget in update-widget")
    ))

(defn is-ancestor? [ancestor child]
  (let [ancestor-count (count ancestor)]
    (cond
      (zero? ancestor-count)
      false

      (= ancestor child)
      true

      (> ancestor-count (count child))
      false

      (= (take ancestor-count child) ancestor)
      true

      :else false
      )))

(defn update-widget? [state widget]
  (let [path (:path widget)]
    (loop [update-set (get-in state this-update-path [])]
      (if (empty? update-set)
        false
        (if (is-ancestor? (first update-set) path)
          true
          (recur (rest update-set)))))))

(defn lock-mouse [state widget]
  (assoc-in state mouse-lock-path (:path widget)))

(defn unlock-mouse [state]
  (assoc-in state mouse-lock-path nil))

(defn get-mouse-lock [state]
  "returns path of locked widget or nil."
  (get-in state mouse-lock-path nil))