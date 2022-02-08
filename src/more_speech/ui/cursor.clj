(ns more-speech.ui.cursor
  (:require [clojure.string :as string]
            [more-speech.ui.graphics :as g])
  )

(defprotocol Cursor
  (set-x [cursor x] "set pixel x position.")
  (set-y [cursor y] "set pixel y position.")
  (set-xy [cursor x y] "set pixel x and y positions.")
  (set-pos [cursor pos] "set horizontal character position.")
  (new-lines [cursor lines] "move cursor down by lines.")
  )

(defn nil->blank [s]
  (if (nil? s)
    ""
    s))

(defrecord cursor [graphics x y l-margin]
  Cursor

  (set-x [c x]
    (assoc c :x x))

  (set-y [c y]
    (assoc c :y y))

  (set-xy [c x y]
    (assoc c :x x :y y))

  (set-pos [c pos]
    (set-x c (g/pos-width graphics pos)))

  (new-lines [c lines]
    (let [dy (* lines (g/line-height graphics))]
      (update c :y + dy)))
  )

(defn draw-text [{:keys [graphics x y l-margin] :as cursor} text]
  (g/text graphics [text (+ x l-margin) y])
  (update cursor :x + (g/text-width graphics text))
  )

(defn draw-line [cursor line]
  (draw-text cursor line)
  (set-xy cursor 0 (+ (:y cursor) (g/line-height (:graphics cursor)))))

(defn draw-multi-line [cursor lines]
  (loop [cursor cursor
         lines (string/split-lines lines)]
    (if (empty? lines)
      cursor
      (recur (draw-line cursor (first lines)) (rest lines)))))

(defn render
  ([cursor window markup]
   (render cursor window markup {}))

  ([cursor window markup artifacts]
   (let [g (:graphics cursor)
         {:keys [bold regular]} (:fonts g)]
     (loop [cursor cursor
            markup markup]
       (cond
         (empty? markup) cursor
         (= :bold (first markup)) (do (g/text-font g bold)
                                      (recur cursor (rest markup)))
         (= :regular (first markup)) (do (g/text-font g regular)
                                         (recur cursor (rest markup)))
         (= :pos (first markup)) (recur (set-pos cursor (second markup))
                                        (drop 2 markup))
         (= :new-line (first markup)) (recur (-> cursor (set-x 0) (new-lines 1))
                                             (rest markup))
         (= :line (first markup)) (do (g/stroke-weight g 1)
                                      (g/line g [0 (:y cursor) (:w window) (:y cursor)])
                                      (recur cursor (rest markup)))
         (= :multi-line (first markup)) (recur (draw-multi-line cursor (second markup))
                                               (drop 2 markup))
         (keyword? (first markup)) (let [k (first markup)
                                         f (get artifacts k)]
                                     (recur (f cursor) (rest markup)))
         (string? (first markup)) (recur (draw-text cursor (first markup))
                                         (rest markup))
         :else (recur (draw-text cursor (.toString (first markup)))
                      (rest markup)))
       ))))