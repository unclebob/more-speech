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
    [more-speech.nostr.trust-updater :as trust-updater]
    [more-speech.nostr.util :as util]
    [more-speech.ui.formatters :as formatters])
  (:use (seesaw [core])))

(defn close-users-frame [users-menu _e]
  (config! users-menu :enabled? true)
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
      (text! widget (first value)))))

(defn load-recent-users []
  (let [after (- (util/get-now) (* 86400 1))
        recent-users (set/difference
                       (set (gateway/get-some-recent-event-authors (get-db) after))
                       (set (get-mem [:user-window :trusted-users])))
        recent-user-items (make-sorted-listbox-items recent-users)]
    (set-mem [:user-window :recent-users] recent-users)
    (set-mem [:user-window :recent-user-items] recent-user-items)))

(defn load-trusted-users []
  (let [trusted-users (contact-list/get-trustees)]
    (set-mem [:user-window :trusted-users] trusted-users)
    (set-mem [:user-window :trusted-user-items]
             (make-sorted-listbox-items trusted-users))))

(defn load-web-of-trust-users []
  (let [web-of-trust-ids (contact-list/get-web-of-trust)
        reduced-web-of-trust-users (set/difference
                                     (set web-of-trust-ids)
                                     (set (get-mem [:user-window :trusted-users])))]
    (set-mem [:user-window :web-of-trust-users] reduced-web-of-trust-users)
    (set-mem [:user-window :web-of-trust-items]
             (make-sorted-listbox-items reduced-web-of-trust-users))))

(defn load-user-window-data []
  (load-trusted-users)
  (load-recent-users))

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
          (let [contact-list (trust-updater/entrust pubkey petname)]
            (when-not (config/is-test-run?)
              (event-composers/compose-and-send-contact-list contact-list))
            (load-trusted-users)
            (let [trusted-items (get-mem [:user-window :trusted-user-items])
                  new-trusted-item (find-item pubkey trusted-items)]
              (config! trusted-listbox :model trusted-items)
              (remove-item pubkey :web-of-trust-items :web-of-trust-users)
              (remove-item pubkey :recent-user-items :recent-users)
              (config! (select frame [:#selected-users])
                       :model (get-mem [:user-window group]))
              (.setSelectedValue trusted-listbox
                                 new-trusted-item true))))))))

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
          (prn 'adding untrusted-user)
          (update-mem [:user-window :recent-users] conj untrusted-user)
          (prn 'recent-users (get-mem [:user-window :recent-users]))
          (set-mem [:user-window :recent-user-items]
                   (make-sorted-listbox-items
                     (get-mem [:user-window :recent-users])))
          (config! selected-listbox :model (get-mem [:user-window :recent-user-items]))
          (.setSelectedValue selected-listbox
                             (find-item untrusted-user (get-mem [:user-window :recent-user-items]))
                             true)
          (when-not (config/is-test-run?)
            (let [my-pubkey (get-mem :pubkey)
                  contacts (gateway/get-contacts (get-db) my-pubkey)]
              (event-composers/compose-and-send-contact-list contacts))))))))

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
                                                                      untrust-button])
                                              selection-panel])]
    (set-mem [:user-window :selection-group] :recent-user-items)
    (load-user-window-data)
    (config! trusted-users-listbox :model (get-mem [:user-window :trusted-user-items]))
    (config! selected-listbox :model (get-mem [:user-window :recent-user-items]))
    (config! users-frame :content users-panel)
    (listen users-frame :window-closing (partial close-users-frame users-menu))
    (config! users-menu :enabled? false)
    (pack! users-frame)
    (show! users-frame)))
