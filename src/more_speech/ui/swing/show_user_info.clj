(ns more-speech.ui.swing.show-user-info
  (:require
    [clojure.string :as string]
    [more-speech.config :as config]
    [more-speech.config :refer [get-db]]
    [more-speech.db.gateway :as gateway]
    [more-speech.mem :as mem]
    [more-speech.nostr.contact-list :as contact-list]
    [more-speech.nostr.protocol :as protocol]
    [more-speech.nostr.trust-updater :as trust-updater]
    [more-speech.nostr.util :as util]
    [more-speech.ui.formatter-util :as formatter-util]
    [more-speech.ui.formatters :as formatters]
    [more-speech.ui.swing.html-util :as html-util]
    [more-speech.ui.swing.tabs :as tabs]
    [more-speech.ui.swing.user-info-interface :as user-info-interface]
    [more-speech.ui.swing.user-info-interface :as html-interface]
    [more-speech.ui.swing.util :as swing-util])
  (:use (seesaw [border] [core]))
  (:import (java.util Timer TimerTask)))

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

(defn- make-profile-menubar [frame id]
  (let [trusted? (some? (contact-list/get-petname id))
        tab-names (vec (remove #(= "all" %) (map :name (mem/get-mem :tabs-list))))
        tab-names (conj tab-names "<new-tab>")
        add-author-actions (map #(action :name % :handler (partial tabs/add-author-to-tab id %)) tab-names)
        trust-item (menu-item
                     :action (action
                               :name (if trusted?
                                       "Untrust user..."
                                       "Trust user...")
                               :handler (fn [_e]
                                          (future
                                            (if trusted?
                                              (untrust id)
                                              (trust id))
                                            (dispose! frame)
                                            (html-interface/show-user-profile id)))))
        add-user-to-tab-item (menu
                               :text "Add user to tab..."
                               :items add-author-actions)
        actions-menu (menu :text "Actions"
                           :items [trust-item
                                   add-user-to-tab-item])]
    (menubar :items [actions-menu])))

(defn render-user-name [widget item]
  (let [id (:value item)]
    (text! widget (formatters/get-best-name id))))

(defn trusted-user-box-click [e]
  (let [user-id (swing-util/get-clicked-value e)
        tab-names (vec (remove #(= "all" %) (map :name (mem/get-mem :tabs-list))))
        tab-names (conj tab-names "<new-tab>")
        add-author-actions (map #(action :name % :handler (partial tabs/add-author-to-tab user-id %)) tab-names)]
    (protocol/request-profiles-and-contacts-for user-id)
    (when (.isPopupTrigger e)
      (let [p (popup :items [(action :name "Get Info..." :handler (fn [_e] (user-info-interface/show-user-profile user-id)))
                             (menu :text "Add author to tab" :items add-author-actions)])]
        (swing-util/show-popup p e)))))

(defn make-trusted-user-pane [id]
  (let [trusted-user-label (label "Trusted Users")
        trusted-users (contact-list/get-trustees id)
        trusted-user-box (listbox :model trusted-users
                                  :renderer render-user-name
                                  :listen [:mouse-pressed trusted-user-box-click])
        trusted-user-pane (vertical-panel :items [trusted-user-label trusted-user-box])]
    (protocol/request-profiles-and-contacts-for trusted-users)
    trusted-user-pane))

(defn- repaint-profile-window [frame]
  (.repaint frame))

(defn show-profile [id profile]
  (let [created-at (:created-at profile)
        petname (contact-list/get-petname id)
        about (string/replace
                (formatter-util/wrap-and-trim
                  (string/replace (:about profile) "\n" " ")
                  80 10)
                "\n"
                "<br>")
        html-doc (make-html-document
                   config/editor-pane-stylesheet
                   (str "<h2> Name:</h2>" (:name profile)
                        "<h2> Petname:</h2>" petname
                        "<h2> Pubkey:</h2>" (util/hexify id)
                        "<h2> About: </h2>" about
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
        trusted-user-pane (make-trusted-user-pane id)
        profile-panel (horizontal-panel :items [(scrollable profile-pane)
                                                (scrollable trusted-user-pane)])
        profile-frame (frame :title (str "User Profile for " (:name profile))
                             :content profile-panel)
        profile-window-timer (Timer. "Profile window timer")
        profile-window-repaint-task (proxy [TimerTask] []
                                      (run [] (repaint-profile-window profile-frame)))
        ]
    (.schedule profile-window-timer profile-window-repaint-task 1000 1000)
    (config! profile-frame :menubar (make-profile-menubar profile-frame id))
    (listen profile-pane :hyperlink html-util/open-link)
    (pack! profile-frame)
    (show! profile-frame)))

(defmethod html-interface/show-user-profile :user-info [id]
  (when-let [profile (gateway/get-profile (get-db) id)]
    (show-profile id profile)))


