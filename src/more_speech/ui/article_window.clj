(ns more-speech.ui.article-window
  (:require [more-speech.ui.text-window :refer [text-window-controls
                                                get-element-height
                                                draw-elements
                                                update-elements]]
            [more-speech.nostr.util :refer [num->hex-string]]
            [more-speech.ui.graphics :as g]
            [more-speech.ui.formatters :as f]
            [more-speech.ui.cursor :as cursor]
            [more-speech.ui.config :as config]
            [more-speech.ui.app-util :as app-util]
            [more-speech.ui.edit-window :as edit]))

(declare get-article-line-height
         draw-article
         update-article
         process-key)

(defrecord article-window-controls []
  text-window-controls
  (get-element-height [_c state]
    (get-article-line-height state))
  (draw-elements [_c state frame]
    (draw-article state frame))
  (update-elements [_c state frame]
    (update-article state frame))
  (key-pressed [_c state frame key]
    (process-key state frame key))
  )

(defn make-article [id name time body]
  {:id id
   :group ""
   :author name
   :subject "?"
   :time time
   :body body}
  )

(defn abbreviate-author [author]
  (f/abbreviate author 20))

(defn markup-article [article]
  [
   :bold
   (abbreviate-author (:author article))
   :pos 30
   (:subject article)
   :regular
   :pos 60
   (f/format-time (:time article))
   :new-line
   :multi-line
   (f/reformat-article (:body article) (:text-width config/article-window-dimensions))
   ])

(defn event->article [text-event nicknames]
  (let [{:keys [id pubkey created-at content]} text-event
        name (get nicknames pubkey (num->hex-string pubkey))
        article (make-article id name created-at content)]
    article))

(defn get-article-line-height [state]
  (let [graphics (app-util/get-graphics state)
        line-height (g/line-height graphics)]
    line-height))

(defn draw-article [state frame]
  (let [application (:application state)
        displayed-article (:displayed-article frame)]
    (when (some? displayed-article)
      (let [g (:graphics application)
            cursor (cursor/->cursor g 0 (g/line-height g) 20)]
        (g/text-align g [:left])
        (g/text-color g config/black)
        (cursor/render cursor frame displayed-article)))))

(defn update-article [state frame]
  (let [application (:application state)
        selected-header (:selected-header application)
        frame (if (nil? selected-header)
                (assoc frame :displayed-article nil
                             :total-elements 0)
                (let [event-map (:text-event-map application)
                      event (get event-map selected-header)
                      nicknames (:nicknames application)
                      marked-up-article (markup-article (event->article event nicknames))]
                  (assoc frame :displayed-article marked-up-article
                               :total-elements 2)))]
    (assoc-in state (:path frame) frame)))

(declare reply-to-article)

(defn process-key [state frame {:keys [_key raw-key]}]
  (condp = (int raw-key)
    config/ctrl-r (reply-to-article state frame)
    state)
  )

(defn reply-to-article [state frame]
  (if (nil? (:displayed-article frame))
    (edit/clear-edit-window state)
    state))

