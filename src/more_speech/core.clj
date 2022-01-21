(ns more-speech.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [more-speech.article :as a]
            [more-speech.text :as text]
            [clojure.data.json :as json]
            [clojure.string :as string]))

(def events (atom []))

(defprotocol widget
  (setup-widget [widget state])
  (update-widget [widget state])
  (draw-widget [widget state])
  (mouse-up [widget state position])
  (mouse-down [widget state position]))

(defrecord article-window [x y w h fonts]
  widget
  (setup-widget [widget state])
  (update-widget [widget state])
  (draw-widget [widget state])
  (mouse-up [widget state position])
  (mouse-down [widget state position])
  )

(defrecord author-window [x y w h fonts]
  widget
  (setup-widget [widget state])
  (update-widget [widget state])
  (draw-widget [widget state])
  (mouse-up [widget state position])
  (mouse-down [widget state position])
  )

(declare draw-author-window)
(declare draw-article-window)

(defrecord application []
  widget
  (setup-widget [widget state])
  (update-widget [widget state])

  (draw-widget [application _]
    (draw-article-window application (:article-window application))
    (draw-author-window application (:author-window application)))

  (mouse-up [widget state position])
  (mouse-down [widget state position])
  )

(defn make-application [bold regular]
  (map->application
    {:articles []
     :nicknames {}

     :article-window (map->article-window
                       {:x 50 :y 10 :w (text/pos-width 105) :h (- (q/screen-height) 100)
                        :fonts {:bold bold :regular regular}
                        })

     :author-window (map->author-window {:x (+ 50 (text/pos-width 110)) :y 10
                                         :w (text/pos-width 30) :h (- (q/screen-height) 100)
                                         :fonts {:bold bold :regular regular}})
     }
    ))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  (let [bold (q/create-font "CourierNewPS-BoldMT" 14)
        regular (q/create-font "CourierNewPSMT" 14)]
    (q/text-font bold)
    {:application (make-application bold regular)}
    ))

(defn make-article [name time body]
  {:group ""
   :author name
   :subject "?"
   :time time
   :body body
   :thread-count 1}
  )

(defn process-event [{:keys [application] :as state} event]
  (let [{:keys [articles nicknames]} application
        name-of (fn [pubkey] (get nicknames pubkey pubkey))
        [name subscription-id inner-event :as decoded-msg] event
        {:strs [id pubkey created_at kind tags content sig]} inner-event]
    (condp = kind
      0 (update-in
          state [:application :nicknames]
          assoc pubkey (get (json/read-str content) "name" "tilt"))
      3 (do (printf "%s: %s %s %s\n" kind (a/format-time created_at) (name-of pubkey) content)
            state)
      1 (assoc-in state [:application :articles]
                  (conj articles
                        (make-article (name-of pubkey) created_at content)))
      4 (do (printf "%s: %s %s %s\n" kind (a/format-time created_at) (name-of pubkey) content)
            state)
      (do (prn "unknown event: " event)
          state)
      )))

(defn update-state [state]
  (if (empty? @events)
    state
    (let [event (first @events)]
      (swap! events rest)
      (process-event state event))
    )
  )

(defn draw-article [window cursor article]
  (q/text-align :left)
  (q/fill 0 0 0)
  (text/render cursor window (a/markup-article article))
  )

(defn draw-articles [application {:keys [fonts] :as window}]
  (loop [cursor (text/->cursor 0 (text/line-height) 5)
         articles (take 20 (:articles application))]
    (if (empty? articles)
      cursor
      (recur (draw-article window cursor (first articles))
             (rest articles)))))

(defn draw-article-window [application window]
  (q/with-translation
    [(:x window) (:y window)]
    (q/stroke 0 0 0)
    (q/stroke-weight 2)
    (q/fill 255 255 255)
    (q/rect 0 0 (:w window) (:h window))
    (draw-articles application window)
    ))

(defn draw-author [window cursor author]
  (q/text-align :left)
  (q/fill 0 0 0)
  (text/render cursor window (a/markup-author author)))

(defn draw-authors [application window]
  (q/text-align :left)
  (q/fill 0 0 0)
  (loop [cursor (text/->cursor 0 (text/line-height) 5)
         authors (take-last 60 (sort-by #(string/lower-case (text/nil->blank (second %))) (:nicknames application)))]
    (if (empty? authors)
      cursor
      (recur (draw-author window cursor (first authors))
             (rest authors))))
  )

(defn draw-author-window [application window]
  (q/with-translation
    [(:x window) (:y window)]
    (q/stroke 0 0 0)
    (q/stroke-weight 2)
    (q/fill 255 255 255)
    (q/rect 0 0 (:w window) (:h window))
    (draw-authors application window)
    ))

(defn draw-state [{:keys [application] :as state}]
  (q/background 240 240 240)
  (draw-widget application state)
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
  (reset! events (read-string (slurp "nostr-messages")))
  args
  )


