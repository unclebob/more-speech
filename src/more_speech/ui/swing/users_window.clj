(ns more-speech.ui.swing.users-window
  (:require [more-speech.mem :refer :all]
            [more-speech.nostr.contact-list :as contact-list]
            [more-speech.ui.formatters :as formatters])
  (:use (seesaw [core])))

(defn close-users-frame [users-menu _e]
  (config! users-menu :enabled? true))

(defn make-users-frame [_e]
  (let [users-menu (select (get-mem :frame) [:#users-menu])
        users-frame (frame :title "Users")
        trusted-users-listbox (listbox :id :trusted-users-listbox)
        trusted-users (contact-list/get-trustees)
        trusted-user-names (sort (map #(formatters/format-user-id % 50 20) trusted-users))
        trusted-users-panel (vertical-panel
                              :items [(label "Trusted")
                                      (scrollable trusted-users-listbox :size [500 :by 800])])
        trust-button (button :text "<-")
        web-of-trust-checkbox (checkbox :text "Web of trust")
        recent-checkbox (checkbox :text "Recent users")
        search-checkbox (checkbox :text "Search")
        search-field (text :text "" :editable? true)
        search-panel (horizontal-panel :items [search-checkbox search-field])
        selected-listbox (listbox)
        web-of-trust-users (contact-list/get-web-of-trust)
        web-of-trust-names (sort (map #(formatters/format-user-id % 50 20) web-of-trust-users))
        scrollable-selected-listbox (scrollable selected-listbox
                                                :size [500 :by 800])
        selection-panel (vertical-panel :items [web-of-trust-checkbox
                                                recent-checkbox
                                                search-panel
                                                scrollable-selected-listbox]
                                        )

        users-panel (horizontal-panel :items [trusted-users-panel trust-button selection-panel])]
    (config! trusted-users-listbox :model trusted-user-names)
    (config! selected-listbox :model web-of-trust-names)
    (config! users-frame :content users-panel)
    (listen users-frame :window-closing (partial close-users-frame users-menu))
    (config! users-menu :enabled? false)
    (pack! users-frame)
    (show! users-frame)))
