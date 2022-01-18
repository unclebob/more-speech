(ns more-speech.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  {
   :article-window {:x 50 :y 10 :w (- (q/screen-width) 100) :h (- (q/screen-height) 100)
                    :articles [{:group "comp.lang.c++"
                                :author "Bob"
                                :time "18 Jan '21 11:12:54 CST"
                                :subject "Subject"
                                :body "My Message to you."
                                :thread-count 15}]
                    :fonts {:bold (q/create-font "Courier New Bold" 14)
                            :regular (q/create-font "Courier New" 14)
                            }
                    }
   })

(defn update-state [state]
  state)

(defprotocol Cursor
  (set-font [cursor font])
  (set-x [cursor x])
  (set-y [cursor y])
  (set-xy [cursor x y])
  )

(defrecord cursor [x y font]
  Cursor
  (set-font [c font]
    (->cursor (:x c) (:y c) font))
  (set-x [c x]
    (->cursor x (:y c) (:font c)))
  (set-y [c y]
    (->cursor (:x c) y (:font c)))
  (set-xy [c x y]
    (->cursor x y (:font c)))
  )

(defn draw-text [{:keys [x y font] :as cursor} text]
  (q/text-font font)
  (q/text text x y)
  (update cursor :x + (q/text-width text))
  )

(defn line-height [] (+ (q/text-ascent) (q/text-descent)))

(defn draw-line [cursor line]
  (draw-text cursor line)
  (set-xy cursor 0 (+ (:y cursor) (line-height))))

(defn draw-article [{:keys [bold regular]} cursor article]
  (q/text-align :left)
  (q/fill 0 0 0)
  (q/with-translation
    [10 10]
    (let [cursor (set-font cursor bold)
          cursor (draw-text cursor (str "* "(:author article)))
          cursor (set-font cursor regular)
          cursor (draw-text cursor (str "(" (:thread-count article) ")"))
          cursor (set-x cursor 100)
          cursor (assoc cursor :font bold)
          cursor (draw-text cursor (:subject article))
          cursor (set-x cursor 500)
          cursor (set-font cursor regular)
          cursor (draw-line cursor (:time article))
          cursor (draw-line cursor (str "  "(:body article)))
          ])))

(defn draw-articles [{:keys [fonts articles]}]
  (let [cursor (->cursor 0 (line-height) (:regular fonts))]
    (doseq [article articles]
      (draw-article fonts cursor article))))

(defn draw-article-window [window]
  (q/with-translation
    [(:x window) (:y window)]
    (q/stroke 0 0 0)
    (q/stroke-weight 2)
    (q/fill 255 255 255)
    (q/rect 0 0 (:w window) (:h window))
    (draw-articles window)
    ))

(defn draw-state [state]
  (q/background 240 240 240)
  (draw-article-window (:article-window state))
  )

(defn ^:export -main [& args]
  (q/defsketch more-speech
               :title "More Speech"
               :size [(q/screen-width) (q/screen-height)]
               :setup setup
               :update update-state
               :draw draw-state
               :middleware [m/fun-mode])
  args
  )

