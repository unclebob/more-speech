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

(defn pos-width [pos]
  (let [s (apply str (repeat pos "X"))]
    (q/text-width s)))

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
    (set-x c (pos-width pos)))

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

(defn render [cursor window markup]
  (let [{:keys [bold regular]} (:fonts window)]
    (loop [cursor cursor
           markup markup]
      (cond
        (empty? markup) cursor
        (= :bold (first markup)) (do (q/text-font bold)
                                     (recur cursor (rest markup)))
        (= :regular (first markup)) (do (q/text-font regular)
                                        (recur cursor (rest markup)))
        (= :pos (first markup)) (recur (set-pos cursor (second markup))
                                       (drop 2 markup))
        (= :new-line (first markup)) (recur (-> cursor (set-x 0) (new-lines 1))
                                            (rest markup))
        (= :line (first markup)) (do (q/stroke-weight 1)
                                     (q/line 0 (:y cursor) (:w window) (:y cursor))
                                     (recur cursor (rest markup)))
        (= :multi-line (first markup)) (recur (draw-multi-line cursor (second markup))
                                              (drop 2 markup))
        (string? (first markup)) (recur (draw-text cursor (first markup))
                                        (rest markup))
        :else (recur (draw-text cursor (.toString (first markup)))
                     (rest markup)))
      )))