(ns more-speech.ui.swing.article-panel
  (:require
    [more-speech.bech32 :as bech32]
    [more-speech.config :refer [get-db]]
    [more-speech.config :as config]
    [more-speech.db.gateway :as gateway]
    [more-speech.mem :refer :all]
    [more-speech.nostr.event-composers :as composers]
    [more-speech.nostr.events :as events]
    [more-speech.nostr.protocol :as protocol]
    [more-speech.nostr.util :as util]
    [more-speech.nostr.zaps :as zaps]
    [more-speech.ui.formatter-util :as formatter-util]
    [more-speech.ui.formatter-util :as f-util]
    [more-speech.ui.formatters :as formatters]
    [more-speech.ui.swing.article-panel-util :as article-panel-util]
    [more-speech.ui.swing.edit-window :as edit-window]
    [more-speech.ui.swing.user-info-interface :as html-interface]
    [more-speech.ui.swing.util :as swing-util :refer [copy-to-clipboard]]
    [more-speech.user-configuration :as uconfig]
    )
  (:use (seesaw [border] [core])))

(defn bold-label [s]
  (label :text s :font (uconfig/get-bold-font)))

(defn popup-label
  ([s popup-f]
   (popup-label s popup-f :nil))

  ([s popup-f id]
   (label :text s
          :font (uconfig/get-bold-font)
          :id id
          :popup popup-f)))

(defn copy-click [e]
  (when (.isPopupTrigger e)
    (let [x (.x (.getPoint e))
          y (.y (.getPoint e))
          node (.getComponent e)
          id (config node :user-data)
          hex-id (util/hexify id)
          note-id (str "nostr:" (bech32/encode "note" id))
          p (popup :items [(action :name (str "Copy " (subs hex-id 0 10) "...")
                                   :handler (partial copy-to-clipboard hex-id))
                           (action :name (str "Copy " (subs note-id 0 16) "...")
                                   :handler (partial copy-to-clipboard note-id))])]
      (.show p (to-widget e) x y))))

(defn id-click [e]
  (if (.isPopupTrigger e)
    (copy-click e)
    (let [id (config e :user-data)]
      (swing-util/select-event id))))

(defn user-name-click [type frame e]
  (let [x (.x (.getPoint e))
        y (.y (.getPoint e))
        node (.getComponent e)
        pubkey (config node :user-data)
        hex-id (util/hexify pubkey)
        npub (bech32/encode "npub" pubkey)
        event-id (config (select frame [:#id-label]) :user-data)
        event (gateway/get-event (get-db) event-id)
        profile (gateway/get-profile (get-db) pubkey)
        popup-items [(action :name (str "Copy " (subs hex-id 0 10) "...")
                             :handler (partial copy-to-clipboard hex-id))
                     (action :name (str "Copy " (subs npub 0 10) "...")
                             :handler (partial copy-to-clipboard npub))]
        popup-items (if (some? profile)
                      (conj popup-items
                            (action :name "Get info..."
                                    :handler (fn [_e] (html-interface/show-user-profile pubkey))))
                      popup-items)
        popup-items (if (and (= type :author)
                             (some? profile))
                      (conj popup-items (action :name "Zap author..."
                                                :handler (partial zaps/zap-author event)))
                      popup-items)
        p (popup :items popup-items)]
    (.show p (to-widget e) x y)))

(defn reaction-click [polarity]
  (let [frame (get-mem :frame)
        up-arrow (select frame [:#up-arrow])
        dn-arrow (select frame [:#dn-arrow])
        event-id (get-mem :selected-event)
        event (gateway/get-event (get-db) event-id)]
    (when (not= (text up-arrow) " ")
      (composers/compose-and-send-reaction-event event polarity))
    (text! up-arrow " ")
    (text! dn-arrow " ")))

(defn up-click [_e]
  (reaction-click "+"))

(defn dn-click [_e]
  (reaction-click "-"))

(declare load-article-info)

(defn reload-article []
  (let [id (get-mem :article-window-event-id)]
    (when (some? id)
      (load-article-info id))))

(defn reaction-items [reactions]
  (loop [reactions reactions
         items [""]]
    (if (empty? reactions)
      items
      (let [[id content] (first reactions)
            name (formatters/format-user-id id 50)]
        (recur (rest reactions) (conj items (str content " " name)))))))

(defn popup-reactions [_e]
  (reaction-items (:reactions (get-mem [:article-panel :event]))))

(defn trim-relay-name [url]
  (re-find config/relay-pattern url))

(defn popup-relays [_e]
  (map trim-relay-name (:relays (get-mem [:article-panel :event]))))

(defn format-zap [[_lnurl zap]]
  (format "%s sats %d - %s"
          (formatter-util/format-time (:created-at zap))
          (:amount zap)
          (:comment zap)))

(defn popup-zaps [_e]
  (map format-zap (:zaps (get-mem [:article-panel :event]))))

(defn popup-mentions [_e]
  (let [event (get-mem [:article-panel :event])
        p-tags (events/get-tag event :p)
        hex-ids (map first p-tags)
        ids (map util/unhexify hex-ids)
        names (map #(formatters/format-user-id % 50) ids)
        ]
    names))

(defn select-reply [id _e]
  (swing-util/select-event id))

(defn popup-replies [_e]
  (let [event (get-mem [:article-panel :event])
        reply-ids (:references event)
        replies (map #(gateway/get-event (get-db) %) reply-ids)
        names (map #(formatters/format-header % :menu-item) replies)
        actions (map #(action :name %1 :handler (partial select-reply %2)) names reply-ids)]
    actions))

(defn make-article-info-panel []
  (let [author-name-label (label :id :author-name-label)
        label-font (uconfig/get-small-font)
        created-time-label (label :id :created-time-label)
        reactions-label (label :id :reactions-count)
        reply-to-label (label :id :reply-to-label)
        id-label (text :id :id-label :editable? false :font label-font)
        citing-label (label :id :citing-label :font label-font)
        subject-label (label :id :subject-label :font label-font)
        root-label (label :id :root-label :font label-font)
        relays-label (label :id :relays-label)
        up-arrow (label :text " " :id :up-arrow)
        dn-arrow (label :text " " :id :dn-arrow)
        zap-icon (label :text " " :id :zap-icon :popup popup-zaps)
        grid
        (grid-panel
          :columns 3
          :preferred-size [-1 :by 70]                       ;icky.
          :items [
                  (flow-panel :align :left :items [(bold-label "Author:") author-name-label zap-icon])
                  (flow-panel :align :left :items [(bold-label "Subject:") subject-label])
                  (flow-panel :align :left :items [(popup-label "Reactions▶" popup-reactions) reactions-label up-arrow dn-arrow])

                  (flow-panel :align :left :items [(bold-label "Created at:") created-time-label])
                  (flow-panel :align :left :items [(popup-label "To: & CC:▶" popup-mentions) reply-to-label])
                  (flow-panel :align :left :items [(popup-label "Relays▶" popup-relays) relays-label])

                  (flow-panel :align :left :items [(bold-label "id:") id-label])
                  (flow-panel :align :left :items [citing-label " " root-label])
                  (flow-panel :align :left :items [(popup-label "Replies▶" popup-replies :replies-label)])])]
    (listen citing-label :mouse-pressed id-click)
    (listen root-label :mouse-pressed id-click)
    (listen id-label :mouse-pressed copy-click)
    (listen up-arrow :mouse-pressed up-click)
    (listen dn-arrow :mouse-pressed dn-click)
    (listen author-name-label :mouse-pressed (partial user-name-click :author grid))
    (listen reply-to-label :mouse-pressed (partial user-name-click :reply-to grid))
    (border-panel :west (label :text "<html><img src=\"http://cleancoder.com/images/cleancodelogo.png\" width=\"100\" height=\"100\"></html>"
                               :id :avatar-id)
                  :center grid)))

(defn make-article-area []
  (editor-pane
    :content-type "text/html"
    :editable? false
    :id :article-area
    :text config/editor-pane-stylesheet))

(defn go-back [_e]
  (article-panel-util/go-back-by 1))

(defn go-forward [_e]
  (article-panel-util/go-back-by -1))

(defn make-control-panel []
  (let [reply-button (button :text "Reply")
        create-button (button :text "Create")
        back-button (button :text "Back")
        forward-button (button :text "Forward")]
    (listen reply-button :action
            (fn [_]
              (edit-window/make-edit-window :reply)))
    (listen create-button :action
            (fn [_] (edit-window/make-edit-window :send)))
    (listen back-button :action go-back)
    (listen forward-button :action go-forward)
    (border-panel :west back-button
                  :east forward-button
                  :center (flow-panel :items [reply-button create-button]))))

(defn has-my-reaction? [event]
  (let [me (get-mem :pubkey)
        reactions (:reactions event)]
    (some #(= me (first %)) reactions)))

(defn- make-html-document [style body]
  (str "<head>" style "</head>"
       "<body>" body "</body>"))

(defn make-article-html [event]
  (make-html-document
    config/editor-pane-stylesheet
    (formatters/reformat-article-into-html
      (formatters/replace-references event))))

(defn load-article-info [selected-id]
  (let [main-frame (get-mem :frame)
        event (gateway/get-event (get-db) selected-id)
        author (:pubkey event)
        profile (gateway/get-profile (get-db) author)
        [root-id _ referent] (events/get-references event)
        replied-event (if (some? referent)
                        (gateway/get-event (get-db) referent)
                        nil)
        root-event-exists? (if (some? root-id)
                             (gateway/event-exists? (get-db) root-id)
                             nil)
        reply-to (select main-frame [:#reply-to-label])
        citing (select main-frame [:#citing-label])
        root-label (select main-frame [:#root-label])
        relays-label (select main-frame [:#relays-label])
        article-area (select main-frame [:#article-area])
        subject-label (select main-frame [:#subject-label])
        up-arrow (select main-frame [:#up-arrow])
        dn-arrow (select main-frame [:#dn-arrow])
        zap-icon (select main-frame [:#zap-icon])
        replies-label (select main-frame [:#replies-label])
        zapped? (some? (:zaps event))
        reacted? (has-my-reaction? event)
        reactions (count (:reactions event))
        reactions-label (select main-frame [:#reactions-count])
        event-id (select main-frame [:#id-label])
        author-name-label (select main-frame [:#author-name-label])
        article-html (make-article-html event)
        new-id? (not= (get-mem :article-window-event-id) selected-id)
        notes-to-request (remove nil? [(if root-event-exists? nil root-id)
                                       (if (some? replied-event) nil referent)])]

    (set-mem [:article-panel :event] event)
    (when new-id?
      (protocol/request-profiles-and-contacts-for [author])
      (when-not (empty? notes-to-request)
        (protocol/request-notes notes-to-request)))
    (set-mem :article-window-event-id selected-id)
    (when (or new-id?
              (not= (get-mem :article-html) article-html))
      (set-mem :article-html article-html)
      (text! article-area article-html))

    (text! reactions-label (str reactions))
    (if reacted?
      (do
        (text! up-arrow " ")
        (text! dn-arrow " "))
      (do
        (text! up-arrow "👍🏻")
        (text! dn-arrow "👎🏻")))
    (if zapped?
      (text! zap-icon "❗⚡ ")                                ;₿ use the bitcoin char?
      (text! zap-icon ""))
    (text! author-name-label
           (formatters/format-user-id author 50))
    (config! author-name-label :user-data author)
    (text! (select main-frame [:#created-time-label])
           (f-util/format-time (:created-at event)))
    (config! event-id
             :user-data (:id event)
             :text (f-util/abbreviate (util/num32->hex-string (:id event)) 20))
    (if (some? referent)
      (let [reply-to-id (:pubkey replied-event)]
        (config! reply-to
                 :user-data reply-to-id
                 :text (formatters/format-user-id reply-to-id 50))
        (config! citing
                 :user-data referent
                 :text (if (nil? replied-event) "[]" "[Citing]")
                 :font (if (nil? replied-event)
                         (uconfig/get-small-font)
                         (uconfig/get-small-bold-font))
                 ))
      (do (text! reply-to "")
          (text! citing "")))
    (if (some? root-id)
      (config! root-label
               :user-data root-id
               :text (cond
                       (= root-id referent) ""
                       root-event-exists? "[Root]"
                       :else "[]")
               :font (if root-event-exists?
                       (uconfig/get-small-bold-font)
                       (uconfig/get-small-font)))
      (text! root-label ""))
    (if (empty? (:references event))
      (text! replies-label "")
      (text! replies-label "Replies▶"))


    (let [avatar-label (select main-frame [:#avatar-id])]
      (future (config! avatar-label
                       :text (if (some? (:picture profile))
                               (format "<html><img src=\"%s\" width=\"100\" height=\"100\"></html>" (:picture profile))
                               "<html><img src=\"http://cleancoder.com/images/cleancodelogo.png\" width=\"100\" height=\"100\"></html>"))))
    (text! subject-label (formatters/get-subject (:tags event)))
    (text! relays-label (format "%d %s"
                                (count (:relays event))
                                (-> event :relays first trim-relay-name (f-util/abbreviate 40))))))
