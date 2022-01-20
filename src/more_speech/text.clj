(ns more-speech.text
  (:require [quil.core :as q]
            [clojure.string :as string])
  )

(defprotocol Cursor
  (text-font [cursor font] "set the font and return the cursor.")
  (set-x [cursor x] "set pixel x position.")
  (set-y [cursor y] "set pixel y position.")
  (set-xy [cursor x y] "set pixel x and y positions.")
  (set-pos [cursor pos] "set horizontal character position.")
  (new-lines [cursor lines] "move cursor down by lines.")
  )


(defn line-height []
  (+ (q/text-ascent) (q/text-descent)))

(defrecord cursor [x y l-margin]
  Cursor
  (text-font [cursor font]
    (q/text-font font)
    cursor)

  (set-x [c x]
    (assoc c :x x))

  (set-y [c y]
    (->cursor (:x c) y (:font c)))

  (set-xy [c x y]
    (assoc c :x x :y y))

  (set-pos [c pos]
    (let [s (apply str (repeat pos "X"))
          x (q/text-width s)]
      (set-x c x)))

  (new-lines [c lines]
    (let [dy (* lines (line-height))]
      (update c :y + dy)))
  )

(defn draw-text [{:keys [x y l-margin] :as cursor} text]
  (q/text text (+ x l-margin) y)
  (update cursor :x + (q/text-width text))
  )

(defn draw-line [cursor line]
  (draw-text cursor line)
  (set-xy cursor 0 (+ (:y cursor) (line-height))))

(defn draw-multi-line [cursor lines]
  (loop [cursor cursor
         lines (string/split-lines lines)]
    (if (empty? lines)
      cursor
      (recur (draw-line cursor (first lines)) (rest lines)))))