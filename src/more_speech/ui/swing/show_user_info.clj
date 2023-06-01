(ns more-speech.ui.swing.show-user-info
  (:require
    [more-speech.config :as config]
    [more-speech.db.gateway :as gateway]
    [more-speech.config :refer [get-db]]
    [more-speech.nostr.contact-list :as contact-list]
    [more-speech.nostr.trust-updater :as trust-updater]
    [more-speech.nostr.util :as util]
    [more-speech.ui.formatter-util :as formatter-util]
    [more-speech.ui.formatters :as formatters]
    [more-speech.ui.swing.html-util :as html-util]
    [more-speech.ui.swing.user-info-interface :as html-interface])
  (:use (seesaw [border] [core])))

(defn trust-author [id _e]
  (trust-updater/trust-this-author id))

(defn- make-html-document [style body]
  (str "<head>" style "</head>"
       "<body>" body "</body>"))

(defn- trust [id]
  (let [petname (trust-updater/ask-for-petname id)]
    (when (some? petname)
      (trust-updater/entrust-and-send id petname))))

(defn- untrust [id]
  (when (= :success (trust-updater/verify-untrust id))
    (trust-updater/untrust-and-send id)))

(defn show-profile [id profile]
  (let [created-at (:created-at profile)
        petname (contact-list/get-petname id)
        html-doc (make-html-document
                   config/editor-pane-stylesheet
                   (str "<h2> Name:</h2>" (:name profile)
                        "<h2> Petname:</h2>" petname
                        "<h2> Pubkey:</h2>" (util/hexify id)
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
        trust-button (button :text (if (some? petname) "Untrust" "Trust"))
        button-panel (flow-panel :items [trust-button])
        profile-panel (vertical-panel :items [button-panel (scrollable profile-pane)])
        profile-frame (frame :title (str "User Profile for " (:name profile))
                             :content profile-panel)]
    (listen profile-pane :hyperlink html-util/open-link)
    (listen trust-button :mouse-pressed (if (some? petname)
                                          (fn [_e]
                                            (untrust id)
                                            (dispose! profile-frame)
                                            (future (html-interface/show-user-profile id)))
                                          (fn [_e]
                                            (trust id)
                                            (dispose! profile-frame)
                                            (future (html-interface/show-user-profile id)))))
    (pack! profile-frame)
    (show! profile-frame)))

(defmethod html-interface/show-user-profile :user-info [id]
  (when-let [profile (gateway/get-profile (get-db) id)]
    (show-profile id profile)))


