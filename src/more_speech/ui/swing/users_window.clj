(ns more-speech.ui.swing.users-window
  (:require
    [clojure.set :as set]
    [more-speech.bech32 :as bech32]
    [more-speech.config :as config]
    [more-speech.config :refer [get-db]]
    [more-speech.db.gateway :as gateway]
    [more-speech.mem :refer :all]
    [more-speech.mem :refer :all]
    [more-speech.nostr.contact-list :as contact-list]
    [more-speech.nostr.event-composers :as event-composers]
    [more-speech.nostr.protocol :as protocol]
    [more-speech.nostr.trust-updater :as trust-updater]
    [more-speech.nostr.util :as util]
    [more-speech.ui.formatters :as formatters]
    [more-speech.ui.swing.article-panel :as article-panel]
    [more-speech.ui.swing.tabs :as tabs])
  (:use (seesaw [core]))
  (:import (java.awt Point)
           (java.util Timer TimerTask)))

(defn close-users-frame [users-menu timer _e]
  (.cancel timer)
  (config! users-menu :enabled? true)
  (when (get-mem [:user-window :contact-list-changed])
    (when-not (config/is-test-run?)
      (let [my-pubkey (get-mem :pubkey)
            contacts (gateway/get-contacts (get-db) my-pubkey)]
        (event-composers/compose-and-send-contact-list contacts)))
    (future (protocol/reconnect-all-relays)))
  (set-mem :user-window nil))

(defn make-sorted-listbox-items [ids]
  (sort-by first
           (map #(vector (formatters/format-user-id % 70 40) %)
                ids))
  )

(defn render-listbox-item [widget item]
  (let [value (:value item)]
    (if (string? value)
      (text! widget value)
      (text! widget (formatters/format-user-id (second value) 70 40)))))

(defn load-recent-users []
  (let [after (- (util/get-now) (* 86400 1))
        recent-users (set/difference
                       (set (gateway/get-some-recent-event-authors (get-db) after))
                       (set (get-mem [:user-window :trusted-users])))
        recent-user-items (make-sorted-listbox-items recent-users)]
    (set-mem [:user-window :recent-users] recent-users)
    (set-mem [:user-window :recent-user-items] recent-user-items)
    (protocol/request-profiles-and-contacts-for recent-users)))

(defn load-trusted-users []
  (let [trusted-users (contact-list/get-trustees)]
    (set-mem [:user-window :trusted-users] trusted-users)
    (set-mem [:user-window :trusted-user-items]
             (make-sorted-listbox-items trusted-users))
    (protocol/request-profiles-and-contacts-for trusted-users)))

(defn load-web-of-trust-users []
  (let [web-of-trust-ids (contact-list/get-web-of-trust)
        reduced-web-of-trust-users (set/difference
                                     (set web-of-trust-ids)
                                     (set (get-mem [:user-window :trusted-users])))]
    (set-mem [:user-window :web-of-trust-users] reduced-web-of-trust-users)
    (set-mem [:user-window :web-of-trust-items]
             (make-sorted-listbox-items reduced-web-of-trust-users))
    (protocol/request-profiles-and-contacts-for reduced-web-of-trust-users)))

(defn load-user-window-data []
  (load-trusted-users)
  (if (= (get-mem [:user-window :selection-group]) :web-of-trust-items)
    (load-web-of-trust-users)
    (load-recent-users)))

(defn reload-user-window-data [frame]
  (load-user-window-data)
  (let [selected-listbox (select frame [:#selected-users])
        trusted-listbox (select frame [:#trusted-users-listbox])
        selected-group (get-mem [:user-window :selection-group])]
    (config! selected-listbox :model (get-mem [:user-window selected-group]))
    (config! trusted-listbox :model (get-mem [:user-window :trusted-user-items]))))

(defn select-recent-users [frame _e]
  (set-mem [:user-window :selection-group] :recent-user-items)
  (load-recent-users)
  (let [selected-listbox (select frame [:#selected-users])]
    (config! selected-listbox :model (get-mem [:user-window :recent-user-items]))))

(defn select-web-of-trust [frame _e]
  (set-mem [:user-window :selection-group] :web-of-trust-items)
  (when (empty? (get-mem [:user-window :web-of-trust-items]))
    (load-web-of-trust-users))
  (config! (select frame [:#selected-users])
           :model (get-mem [:user-window :web-of-trust-items])))

(defn remove-item [id items-tag ids-tag]
  (let [items (remove #(= id (second %)) (get-mem [:user-window items-tag]))
        ids (remove #(= id %) (get-mem [:user-window ids-tag]))]
    (set-mem [:user-window items-tag] items)
    (set-mem [:user-window ids-tag] ids)))

(defn find-item [id items]
  (loop [items items]
    (if (empty? items)
      nil
      (let [item (first items)]
        (if (= id (second item))
          item
          (recur (rest items)))))))

(defn trust-selection [frame _e]
  (let [selected-listbox (select frame [:#selected-users])
        selected-item (selection selected-listbox)
        group (get-mem [:user-window :selection-group])]
    (when (some? selected-item)
      (let [pubkey (second selected-item)
            petname (trust-updater/ask-for-petname pubkey)
            trusted-listbox (select frame [:#trusted-users-listbox])]
        (when (some? petname)
          (trust-updater/entrust pubkey petname)
          (set-mem [:user-window :contact-list-changed] true)
          (load-trusted-users)
          (let [trusted-items (get-mem [:user-window :trusted-user-items])
                new-trusted-item (find-item pubkey trusted-items)]
            (config! trusted-listbox :model trusted-items)
            (remove-item pubkey :web-of-trust-items :web-of-trust-users)
            (remove-item pubkey :recent-user-items :recent-users)
            (config! (select frame [:#selected-users])
                     :model (get-mem [:user-window group]))
            (.setSelectedValue trusted-listbox
                               new-trusted-item true)))))))

(defn untrust-selection [frame _e]
  (let [trusted-listbox (select frame [:#trusted-users-listbox])
        selected-item (selection trusted-listbox)]
    (when (some? selected-item)
      (let [untrusted-user (second selected-item)
            recent-button (select frame [:#recent-button])
            selected-listbox (select frame [:#selected-users])
            untrusted-name (formatters/format-user-id untrusted-user)
            question (dialog :content
                             (str "Are you sure you want to untrust " untrusted-name
                                  "\nID: " (util/hexify untrusted-user)
                                  "\n" (bech32/encode "npub" untrusted-user))
                             :option-type :ok-cancel)
            answer (show! (pack! question))]
        (when (= answer :success)
          (trust-updater/untrust untrusted-user)
          (load-trusted-users)
          (config! trusted-listbox :model (get-mem [:user-window :trusted-user-items]))
          (config! recent-button :selected? true)
          (load-recent-users)
          (update-mem [:user-window :recent-users] conj untrusted-user)
          (set-mem [:user-window :recent-user-items]
                   (make-sorted-listbox-items
                     (get-mem [:user-window :recent-users])))
          (config! selected-listbox :model (get-mem [:user-window :recent-user-items]))
          (.setSelectedValue selected-listbox
                             (find-item untrusted-user (get-mem [:user-window :recent-user-items]))
                             true)
          (set-mem [:user-window :contact-list-changed] true))))))

(defn listbox-click [listbox e]
  (let [index (.locationToIndex listbox (Point. (.getX e) (.getY e)))
        model (.getModel listbox)
        item (.getElementAt model index)
        user-id (second item)
        tab-names (vec (remove #(= "all" %) (map :name (get-mem :tabs-list))))
        tab-names (conj tab-names "<new-tab>")
        add-author-actions (map #(action :name % :handler (partial tabs/add-author-to-tab user-id %)) tab-names)]
    (protocol/request-profiles-and-contacts-for user-id)
    (when (.isPopupTrigger e)
      (let [p (popup :items [(action :name "Get Info..." :handler (fn [_e] (article-panel/show-user-profile user-id)))
                             (menu :text "Add author to tab" :items add-author-actions)])]
        (.show p (to-widget e) (.x (.getPoint e)) (.y (.getPoint e)))))))

(defn- repaint-user-window [frame]
  (.repaint frame))

(defn make-users-frame [_e]
  (let [users-menu (select (get-mem :frame) [:#users-menu])
        users-frame (frame :title "Users")
        trusted-users-listbox (listbox :id :trusted-users-listbox
                                       :font config/default-font
                                       :renderer render-listbox-item)
        trusted-users-panel (vertical-panel
                              :items [(label "Trusted")
                                      (scrollable trusted-users-listbox :size [500 :by 800])])
        trust-button (button :text "<-Trust"
                             :listen [:action (partial trust-selection users-frame)])
        untrust-button (button :text "Untrust->"
                               :listen [:action (partial untrust-selection users-frame)])
        reload-button (button :text "Reload"
                              :listen [:action (fn [_e] (reload-user-window-data users-frame))])
        selection-group (button-group)
        web-of-trust-button (radio :text "Web of trust"
                                   :group selection-group
                                   :selected? false
                                   :listen [:action (partial select-web-of-trust users-frame)])
        recent-button (radio :text "Recent users"
                             :id :recent-button
                             :group selection-group
                             :selected? true
                             :listen [:action (partial select-recent-users users-frame)])

        selected-listbox (listbox :id :selected-users
                                  :font config/default-font
                                  :renderer render-listbox-item)
        scrollable-selected-listbox (scrollable selected-listbox
                                                :size [500 :by 800])
        selection-panel (vertical-panel :items [web-of-trust-button
                                                recent-button
                                                scrollable-selected-listbox])


        users-panel (horizontal-panel :items [trusted-users-panel
                                              (vertical-panel :items [trust-button
                                                                      untrust-button
                                                                      reload-button])
                                              selection-panel])
        user-window-timer (Timer. "User window timer")
        user-window-repaint-task (proxy [TimerTask] []
                                   (run [] (repaint-user-window users-frame)))]
    (set-mem [:user-window :selection-group] :recent-user-items)
    (load-user-window-data)
    (config! trusted-users-listbox :model (get-mem [:user-window :trusted-user-items]))
    (config! selected-listbox :model (get-mem [:user-window :recent-user-items]))
    (config! users-frame :content users-panel)
    (listen users-frame :window-closing (partial close-users-frame users-menu user-window-timer))
    (listen trusted-users-listbox :mouse-pressed (partial listbox-click trusted-users-listbox))
    (listen selected-listbox :mouse-pressed (partial listbox-click selected-listbox))
    (config! users-menu :enabled? false)
    (.schedule user-window-timer user-window-repaint-task 1000 1000)
    (pack! users-frame)
    (show! users-frame)))
