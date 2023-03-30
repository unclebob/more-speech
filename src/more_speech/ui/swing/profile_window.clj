(ns more-speech.ui.swing.profile-window
  (:require
    [more-speech.bech32 :as bech32]
    [more-speech.config :as config]
    [more-speech.mem :refer :all]
    [more-speech.nostr.elliptic-signature :as es]
    [more-speech.nostr.util :as util])
  (:use (seesaw [core])))

(defn close-profile-frame [menu _e]
  (config! menu :enabled? true))

(defn get-text-from-frame [frame id]
  (config (select frame [id]) :text))

(defn validate-private-key [private-key]
  (if (re-matches config/hex-key-pattern private-key)
    (util/unhexify private-key)
    (if-not (.startsWith private-key "nsec")
      "Bad format."
      (try
        (bech32/address->number private-key)
        (catch Exception e
          (.getMessage e))))))

(defn private-key-valid? [private-key]
  (if (empty? private-key)
    true
    (let [validation (validate-private-key private-key)]
      (if (number? validation)
        (let [question (dialog :content (str "Change private key to: "
                                             private-key "?")
                               :option-type :ok-cancel)
              answer (show! (pack! question))
              pubkey (util/bytes->num (es/get-pub-key (util/num->bytes 32 validation)))]
          (if (= answer :success)
            (do
              (alert ["changing private key to: " (text :text private-key) "\n"
                      "public-key: " (text :text (util/hexify pubkey)) "\n"
                      "npub: " (text :text (bech32/encode "npub" pubkey))])
              true)
            false))
        (do (alert (str "Private key invalid: " validation))
            false)))))

(defn name-valid? [name]
  (cond
    (empty? name)
    (do (alert "Enter a name.") false)

    (not (re-matches config/user-name-chars name))
    (do (alert "Name can only contain letters, numbers, and -.") false)

    (> (count name) 20)
    (do (alert "Name cannot be more than 20 characters.") false)

    :else true))

(defn validate-and-save-profile [profile-frame]
  (let [get-text (partial get-text-from-frame profile-frame)
        name (get-text :#name-field)
        about (get-text :#about-field)
        picture (get-text :#picture-field)
        nip05 (get-text :#nip05-field)
        lud16 (get-text :#lud16-field)
        private-key (get-text :#private-key-field)
        valid? (and true
                    (private-key-valid? private-key)
                    (name-valid? name))
        ]
    (when valid?
      (close-profile-frame (select (get-mem :frame) [:#profile-menu]) nil)
      (dispose! profile-frame))
    )
  )

(defn make-data-panel [field-name content id editable?]
  (let [the-label (label :text field-name :size [150 :by 20])
        the-field (text :text content
                        :editable? editable?
                        :id id
                        :size [800 :by 20])]
    (left-right-split the-label the-field)))

(defn make-profile-frame [_e]
  (let [profile-menu (select (get-mem :frame) [:#profile-menu])
        profile-frame (frame :title "Profile")
        name-panel (make-data-panel "Name: "
                                    (get-mem [:keys :name])
                                    :name-field true)
        about-panel (make-data-panel "About:"
                                     (get-mem [:keys :about])
                                     :about-field true)
        picture-panel (make-data-panel "Picture:"
                                       (get-mem [:keys :picture])
                                       :picture-field true)
        pubkey-panel (make-data-panel "Public key (hex):"
                                      (get-mem [:keys :public-key])
                                      :pubkey-field false)
        npub-panel (make-data-panel "Public key (npub)"
                                    (bech32/encode "npub" (util/unhexify (get-mem [:keys :public-key])))
                                    :npub-field false)
        nip05-panel (make-data-panel "NIP05 Internet Id:"
                                     (get-mem [:keys :nip05])
                                     :nip05-field
                                     true)
        lud16-panel (make-data-panel "LUD16 Zap Address:"
                                     (get-mem [:keys :lud16])
                                     :lud16-field
                                     true)
        private-key-panel (make-data-panel "Private Key:"
                                           ""
                                           :private-key-field
                                           true)
        ok-button (button :text "OK"
                          :listen [:action (fn [_e] (validate-and-save-profile profile-frame))])
        cancel-button (button :text "Cancel"
                              :listen [:action (partial close-profile-frame profile-menu)
                                       :action (fn [_e] (dispose! profile-frame))])
        button-panel (horizontal-panel :items [cancel-button ok-button])
        profile-panel (vertical-panel :items [name-panel
                                              about-panel
                                              picture-panel
                                              pubkey-panel
                                              npub-panel
                                              nip05-panel
                                              lud16-panel
                                              private-key-panel
                                              button-panel])]
    (config! profile-frame :content profile-panel)
    (config! profile-menu :enabled? false)
    (listen profile-frame :window-closing (partial close-profile-frame profile-menu))
    (pack! profile-frame)
    (show! profile-frame)))
