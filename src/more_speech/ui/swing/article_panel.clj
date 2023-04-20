(ns more-speech.ui.swing.article-panel
  (:require [clojure.java.browse :as browse]
            [clojure.string :as string]
            [more-speech.bech32 :as bech32]
            [more-speech.config :refer [get-db]]
            [more-speech.config :as config]
            [more-speech.db.gateway :as gateway]
            [more-speech.logger.default :refer [log-pr]]
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
            [more-speech.ui.swing.util :as swing-util :refer [copy-to-clipboard]]
            [more-speech.user-configuration :as uconfig]
            [seesaw.mouse :as mouse])
  (:use (seesaw [border] [core]))
  (:import (javax.swing.event HyperlinkEvent$EventType)))

(declare open-link)

(defn bold-label [s]
  (label :text s :font (uconfig/get-bold-font)))

(defn copy-click [e]
  (when (.isPopupTrigger e)
    (let [x (.x (.getPoint e))
          y (.y (.getPoint e))
          node (.getComponent e)
          id (config node :user-data)
          hex-id (util/hexify id)
          note-id (bech32/encode "note" id)
          p (popup :items [(action :name (str "Copy " (subs hex-id 0 10) "...")
                                   :handler (partial copy-to-clipboard hex-id))
                           (action :name (str "Copy " (subs note-id 0 10) "...")
                                   :handler (partial copy-to-clipboard note-id))])]
      (.show p (to-widget e) x y))))

(defn id-click [e]
  (if (.isPopupTrigger e)
    (copy-click e)
    (swing-util/select-event (config e :user-data))))

(defn make-html-document [style body]
  (str "<head>" style "</head>"
       "<body>" body "</body>"))

(defn show-profile [profile]
  (let [created-at (:created-at profile)
        html-doc (make-html-document
                   config/editor-pane-stylesheet
                   (str "<h2> Name:</h2>" (:name profile)
                        "<h2> About: </h2>" (:about profile)
                        "<h2> Display name: </h2>" (:display-name profile)
                        "<h2> Banner: </h2>" (:banner profile)
                        "<h2> Website: </h2>" (:website profile)
                        "<h2> Zap Addr: </h2>" (:lud16 profile) " " (:lud06 profile)
                        "<h2> Identifier: </h2>" (:nip05 profile)
                        "<h2> As of: </h2>" (if (nil? created-at) "?" (formatter-util/format-time created-at))
                        "<p><img src=\"" (:picture profile) "\" width=\"350\">"
                        "<p>" (formatters/linkify (:picture profile))
                        "<p>" (apply str (keys profile)))
                   )
        profile-pane (editor-pane
                       :content-type "text/html"
                       :editable? false
                       :id :article-area
                       :text html-doc)
        profile-frame (frame :title (str "User Profile for " (:name profile))
                             :content (scrollable profile-pane)
                             :size [500 :by 600])]
    (listen profile-pane :hyperlink open-link)

    (show! profile-frame)))


(defn show-user-profile [id]
  (when-let [profile (gateway/get-profile (get-db) id)]
    (show-profile profile)))

(defn user-name-click [type frame e]
  (let [x (.x (.getPoint e))
        y (.y (.getPoint e))
        node (.getComponent e)
        pubkey (config node :user-data)
        hex-id (util/hexify pubkey)
        npub (bech32/encode "npub" pubkey)
        event-id (config (select frame [:#id-label]) :user-data)
        event (gateway/get-event (get-db) event-id)
        popup-items [(action :name (str "Copy " (subs hex-id 0 10) "...")
                             :handler (partial copy-to-clipboard hex-id))
                     (action :name (str "Copy " (subs npub 0 10) "...")
                             :handler (partial copy-to-clipboard npub))
                     (action :name "Get info..."
                             :handler (fn [_e] (show-user-profile pubkey)))]
        popup-items (if (= type :author)
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

(defn make-article-info-panel []
  (let [author-name-label (label :id :author-name-label)
        label-font (uconfig/get-small-font)
        created-time-label (label :id :created-time-label)
        reactions-popup (popup :enabled? false)
        reactions-label (label :id :reactions-count :user-data reactions-popup)
        reply-to-label (label :id :reply-to-label)
        id-label (text :id :id-label :editable? false :font label-font)
        citing-label (text :id :citing-label :editable? false :font label-font)
        subject-label (label :id :subject-label :font label-font)
        root-label (text :id :root-label :editable? false :font label-font)
        relays-popup (popup :enabled? false)
        relays-label (label :id :relays-label :user-data relays-popup)
        up-arrow (label :text " " :id :up-arrow)
        dn-arrow (label :text " " :id :dn-arrow)
        zaps-popup (popup :enabled? false)
        zap-icon (label :text " " :id :zap-icon :user-data zaps-popup)
        grid
        (grid-panel
          :columns 3
          :preferred-size [-1 :by 70]                       ;icky.
          :items [
                  (flow-panel :align :left :items [(bold-label "Author:") author-name-label zap-icon])
                  (flow-panel :align :left :items [(bold-label "Subject:") subject-label])
                  (flow-panel :align :left :items [(bold-label "Reactions:") reactions-label up-arrow dn-arrow])

                  (flow-panel :align :left :items [(bold-label "Created at:") created-time-label])
                  (flow-panel :align :left :items [(bold-label "Reply to:") reply-to-label])
                  (flow-panel :align :left :items [(bold-label "Relays:") relays-label])

                  (flow-panel :align :left :items [(bold-label "id:") id-label])
                  (flow-panel :align :left :items [(bold-label "Citing:") citing-label])
                  (flow-panel :align :left :items [(bold-label "Root:") root-label])])]
    (listen relays-label
            :mouse-entered (fn [e]
                             (-> relays-popup
                                 (move! :to (.getLocationOnScreen e))
                                 show!))
            :mouse-exited (fn [_e] (hide! relays-popup)))
    (listen reactions-label
            :mouse-entered (fn [e]
                             (-> reactions-popup
                                 (move! :to (.getLocationOnScreen e))
                                 show!))
            :mouse-exited (fn [_e] (hide! reactions-popup)))

    (listen zap-icon
            :mouse-entered (fn [e]
                             (-> zaps-popup
                                 (move! :to (.getLocationOnScreen e))
                                 show!))
            :mouse-exited (fn [_e] (hide! zaps-popup)))
    (listen citing-label :mouse-pressed id-click)
    (listen root-label :mouse-pressed id-click)
    (listen id-label :mouse-pressed copy-click)
    (listen up-arrow :mouse-pressed up-click)
    (listen dn-arrow :mouse-pressed dn-click)
    (listen author-name-label :mouse-pressed (partial user-name-click :author grid))
    (listen reply-to-label :mouse-pressed (partial user-name-click :reply-to grid))
    grid))

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

(defn reaction-items [reactions]
  (loop [reactions reactions
         items [""]]
    (if (empty? reactions)
      items
      (let [[id content] (first reactions)
            name (formatters/format-user-id id 50)]
        (recur (rest reactions) (conj items (str content " " name)))))))

(defn make-article-html [event]
  (make-html-document
    config/editor-pane-stylesheet
    (formatters/reformat-article-into-html
      (formatters/replace-references event))))

(defn format-zap [[_lnurl zap]]
  (format "%s sats %d - %s"
          (formatter-util/format-time (:created-at zap))
          (:amount zap)
          (:comment zap)))

(defn load-article-info [selected-id]
  (let [main-frame (get-mem :frame)
        event (gateway/get-event (get-db) selected-id)
        [root-id _ referent] (events/get-references event)
        reply-to (select main-frame [:#reply-to-label])
        citing (select main-frame [:#citing-label])
        root-label (select main-frame [:#root-label])
        relays-label (select main-frame [:#relays-label])
        relays-popup (config relays-label :user-data)
        article-area (select main-frame [:#article-area])
        subject-label (select main-frame [:#subject-label])
        up-arrow (select main-frame [:#up-arrow])
        dn-arrow (select main-frame [:#dn-arrow])
        zap-icon (select main-frame [:#zap-icon])
        zaps-popup (config zap-icon :user-data)
        zapped? (some? (:zaps event))
        reacted? (has-my-reaction? event)
        reactions (count (:reactions event))
        reactions-label (select main-frame [:#reactions-count])
        reactions-popup (config reactions-label :user-data)
        relay-names (map #(re-find config/relay-pattern %) (:relays event))
        zap-items (map format-zap (:zaps event))
        event-id (select main-frame [:#id-label])
        author-name-label (select main-frame [:#author-name-label])]
    (protocol/request-profiles-and-contacts-for (:pubkey event))
    (text! reactions-label (str reactions))
    (if reacted?
      (do
        (text! up-arrow " ")
        (text! dn-arrow " "))
      (do
        (text! up-arrow "👍🏻")
        (text! dn-arrow "👎🏻")))
    (if zapped?
      (text! zap-icon "❗ ")
      (text! zap-icon ""))
    (swing-util/clear-popup relays-popup)
    (swing-util/clear-popup reactions-popup)
    (swing-util/clear-popup zaps-popup)
    (config! relays-popup :items relay-names)
    (config! reactions-popup :items (reaction-items (:reactions event)))
    (config! zaps-popup :items zap-items)
    (text! article-area (make-article-html event))
    (text! author-name-label
           (formatters/format-user-id (:pubkey event) 50))
    (config! author-name-label :user-data (:pubkey event))
    (text! (select main-frame [:#created-time-label])
           (f-util/format-time (:created-at event)))
    (config! event-id
             :user-data (:id event)
             :text (f-util/abbreviate (util/num32->hex-string (:id event)) 20))
    (if (some? referent)
      (let [replied-event (gateway/get-event (get-db) referent)
            reply-to-id (:pubkey replied-event)]
        (config! reply-to
                 :user-data reply-to-id
                 :text (formatters/format-user-id reply-to-id 50))
        (config! citing
                 :user-data referent
                 :text (f-util/abbreviate (util/num32->hex-string referent) 20)
                 :font (if (nil? replied-event)
                         (uconfig/get-small-font)
                         (uconfig/get-small-bold-font))
                 ))
      (do (text! reply-to "")
          (text! citing "")))
    (if (some? root-id)
      (let [root-event-exists? (gateway/event-exists? (get-db) root-id)]
        (config! root-label
                 :user-data root-id
                 :text (f-util/abbreviate (util/num32->hex-string root-id) 20)
                 :font (if root-event-exists?
                         (uconfig/get-small-bold-font)
                         (uconfig/get-small-font))))
      (text! root-label ""))
    (text! subject-label (formatters/get-subject (:tags event)))
    (text! relays-label (format "%d %s"
                                (count relay-names)
                                (f-util/abbreviate (first relay-names) 40)))))


(defn get-user-id-from-subject [subject]
  (cond
    (.startsWith subject "@")
    (composers/find-user-id (subs subject 1))

    (.startsWith subject "npub")
    (bech32/address->number subject)

    :else nil))

(defn get-user-info [subject _e]
  (when-let [id (get-user-id-from-subject subject)]
    (show-user-profile id)))

(defn pop-up-name-menu [e subject]
  (let [p (popup :items [(action :name "Get Info..."
                                 :handler (partial get-user-info subject))])
        ev (.getInputEvent e)
        [x y] (mouse/location ev)]
    (.show p (to-widget e) x y)))

;---Declared
(defn open-link [e]
  (when (= HyperlinkEvent$EventType/ACTIVATED (.getEventType e))
    (when-let [url (str (.getURL e))]
      (let [[type subject] (string/split (.getDescription e) #"://")]
        (cond
          (or (= type "http") (= type "https"))
          (try
            (browse/browse-url url)
            (catch Exception ex
              (log-pr 1 'open-link url (.getMessage ex))
              (log-pr 1 ex)))

          (= type "ms-idreference")
          (let [id (util/unhexify (subs subject 1))]
            (swing-util/select-event id))

          (= type "ms-namereference")
          (pop-up-name-menu e subject)

          :else
          (do (log-pr 1 'open-link url 'type type 'subject subject)
              (log-pr 1 (.getDescription e)))
          )))))
