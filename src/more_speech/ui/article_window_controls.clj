(ns more-speech.ui.article-window-controls
  (:require [more-speech.ui.text-window :refer [text-window-controls
                                                get-element-height
                                                draw-elements
                                                update-elements]]
            [more-speech.nostr.util :refer [num->hex-string]]
            [more-speech.ui.graphics :as g]
            [more-speech.content.article :as a]
            [more-speech.ui.cursor :as cursor]))

(declare get-article-line-height
         draw-article
         update-article)

(defrecord article-window-controls []
  text-window-controls
  (get-element-height [_c state]
    (get-article-line-height state))
  (draw-elements [_c state frame]
    (draw-article state frame))
  (update-elements [_c state frame]
    (update-article state frame))
  )

(defn make-article [id name time body]
  {:id id
   :group ""
   :author name
   :subject "?"
   :time time
   :body body}
  )

(defn markup-article [article]
  [
   :bold
   (a/abbreviate-author (:author article))
   :pos 30
   (:subject article)
   :regular
   :pos 60
   (a/format-time (:time article))
   :new-line
   (:body article)
   ])

(defn event->article [text-event nicknames]
  (let [{:keys [id pubkey created-at content]} text-event
        name (get nicknames pubkey (num->hex-string pubkey))
        article (make-article id name created-at content)]
    article
    )
  )

(defn get-article-line-height [state]
  (let [graphics (get-in state [:application :graphics])
        line-height (g/line-height graphics)]
    line-height))

(defn draw-article [state frame]
  (let [application (:application state)
        selected-header (:selected-header application)]
    (when (some? selected-header)
      (let [g (:graphics application)
            event-map (:text-event-map application)
            event (get event-map selected-header)
            nicknames (:nicknames application)
            marked-up-article (markup-article (event->article event nicknames))
            cursor (cursor/->cursor g 0 (g/line-height g) 20)]
        (g/text-align g [:left])
        (g/text-color g [0 0 0])
        (cursor/render cursor frame marked-up-article)))))

(defn update-article [state frame]
  (if (some? (get-in state [:application :selected-header]))
    (assoc-in state (concat (:path frame) [:total-elements]) 2) ;hack
    (assoc-in state (concat (:path frame) [:total-elements]) 0))
  )