(ns more-speech.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [more-speech.text
             :as text
             :refer [text-font
                     draw-text
                     set-pos
                     draw-line
                     new-lines
                     draw-lines]]))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  {
   :article-window {:x 50 :y 10 :w (- (q/screen-width) 100) :h (- (q/screen-height) 100)
                    :articles [{:group "comp.lang.c++"
                                :author "Bob"
                                :time "18 Jan '22 11:12:54 CST"
                                :subject "Subject"
                                :body "My Message to you."
                                :thread-count 15}
                               {:group "comp.object"
                                :author "Robert C. Martin"
                                :time "19 Jan '22 12:54:51"
                                :subject "The Subject is up to you."
                                :body "The quick brown fox jumped over\nthe lazy dog's back."
                                :thread-count 152}]
                    :fonts {:bold (q/create-font "CourierNewPS-BoldMT" 14)
                            :regular (q/create-font "CourierNewPSMT" 14)
                            }
                    }
   })

(defn update-state [state]
  state)

(defn draw-article [window cursor article]
  (q/text-align :left)
  (q/fill 0 0 0)
  (let [{:keys [bold regular]} (:fonts window)
        cursor (-> cursor
                   (text-font bold)
                   (draw-text (str "* " (:author article)))
                   (text-font regular)
                   (draw-text (str " (" (:thread-count article) ")"))
                   (text-font bold)
                   (set-pos 40)
                   (draw-text (:subject article))
                   (text-font regular)
                   (set-pos 80)
                   (draw-line (:time article))
                   (draw-lines (:body article)))]
    (q/stroke-weight 1)
    (q/line 0 (:y cursor) (:w window) (:y cursor))
    (new-lines cursor 1)))

(defn draw-articles [{:keys [fonts articles] :as window}]
  (loop [cursor (text/->cursor 0 (text/line-height) 5)
         articles articles]
    (if (empty? articles)
      cursor
      (recur (draw-article window cursor (first articles))
             (rest articles)))))

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

(declare more-speech)
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

