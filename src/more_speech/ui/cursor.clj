(ns more-speech.ui.cursor
  (:require [clojure.string :as string]
            [more-speech.ui.graphics :as g]
            [more-speech.ui.config :as config])
  )

(defprotocol Cursor
  (set-x [cursor x] "set pixel x position.")
  (set-y [cursor y] "set pixel y position.")
  (set-xy [cursor x y] "set pixel x and y positions.")
  (set-pos [cursor pos] "set horizontal character position.")
  (new-lines [cursor lines] "move cursor down by lines.")
  (draw [cursor] "draw the flashing text cursor")
  (draw-text [cursor text] "draw text and update cursor")
  )

(defn nil->blank [s]
  (if (nil? s)
    ""
    s))

(defrecord cursor [graphics x y l-margin]
  Cursor

  (set-x [c new-x]
    (assoc c :x new-x))

  (set-y [c new-y]
    (assoc c :y new-y))

  (set-xy [c new-x new-y]
    (assoc c :x new-x :y new-y))

  (set-pos [c pos]
    (set-x c (g/pos-width graphics pos)))

  (new-lines [c lines]
    (let [dy (* lines (g/line-height graphics))]
      (update c :y + dy)))

  (draw [c]
    (let [cx (+ x l-margin)
          line-height (* (g/line-height graphics) 0.6)]
      (when (< (mod (g/get-time graphics) 1000) 500)
        (g/stroke graphics config/black)
        (g/stroke-weight graphics 1)
        (g/line graphics [cx (- y line-height) cx y])))
    c)

  (draw-text [{:keys [graphics x y l-margin] :as cursor} text]
    (g/text graphics [text (+ x l-margin) y])
    (update cursor :x + (g/text-width graphics text))
    )
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

(defn valid-markup-token? [token]
  (or
    (keyword? token)
    (string? token)
    (number? token)))

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
         (string? (first markup)) (let [the-string (first (string/split-lines (first markup)))]
                                    (recur (draw-text cursor the-string)
                                           (rest markup)))
         :else (recur (draw-text cursor (prn-str 'unknown (first markup)))
                      (rest markup)))
       ))))