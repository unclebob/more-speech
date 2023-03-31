(ns more-speech.ui.swing.profile-window
  (:require
    [more-speech.bech32 :as bech32]
    [more-speech.config :as config]
    [more-speech.data-storage :as data-storage]
    [more-speech.mem :refer :all]
    [more-speech.nostr.elliptic-signature :as es]
    [more-speech.nostr.event-composers :as event-composers]
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

(defn query-and-change-private-key [private-key-number private-key-string]
  (let [question (dialog :content (str "Change private key to: "
                                       private-key-string "?")
                         :option-type :ok-cancel)
        answer (show! (pack! question))
        pubkey (util/bytes->num (es/get-pub-key (util/num->bytes 32 private-key-number)))]
    (if (= answer :success)
      (do
        (alert ["changing private key to:"
                (text :text private-key-string)
                (text :text (bech32/encode "nsec" private-key-number))
                "\n"
                "public-key:"
                (text :text (util/hexify pubkey))
                (text :text (bech32/encode "npub" pubkey))])
        true)
      false)))

(defn private-key-valid? [private-key]
  (if (empty? private-key)
    true
    (let [validation (validate-private-key private-key)]
      (if (number? validation)
        (do
          (if (= validation (util/unhexify (get-mem [:keys :private-key])))
            true
            (query-and-change-private-key validation private-key)))
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

(defn about-valid? [about]
  (cond
    (empty? about)
    (do (alert "About: Gosh, at least say hi.") false)

    (> (count about) 256)
    (do (alert "About: Maybe don't say quite so much.  256 chars should be enough.") false)

    :else true))

(defn picture-valid? [picture]
  (cond
    (empty? picture)
    true

    (= picture (get-mem [:keys :picture]))
    true?

    (re-matches config/url-pattern picture)
    true

    :else
    (do (alert "Picture must be a url of some kind.") false))
  )

(defn nip05-valid? [nip05]
  (cond
    (empty? nip05)
    true

    (= nip05 (get-mem [:keys :nip05]))
    true

    (re-matches config/email-pattern nip05)
    true

    :else
    (do (alert "Nip-05 internet identifier must look like an email address.") false)))

(defn lud16-valid? [lud16]
  (cond
    (empty? lud16)
    true

    (= lud16 (get-mem [:keys :lud16]))
    true

    (re-matches config/email-pattern lud16)
    true

    :else
    (do (alert "LUD-16 Zap address must look like an email address.") false)))

(defn validate-and-save-profile [profile-frame]
  (let [get-text (partial get-text-from-frame profile-frame)
        name (get-text :#name-field)
        about (get-text :#about-field)
        picture (get-text :#picture-field)
        nip05 (get-text :#nip05-field)
        lud16 (get-text :#lud16-field)
        private-key (get-text :#private-key-field)
        valid? (and (private-key-valid? private-key)
                    (name-valid? name)
                    (about-valid? about)
                    (picture-valid? picture)
                    (nip05-valid? nip05)
                    (lud16-valid? lud16))
        ]
    (when valid?
      (let [private-key (validate-private-key private-key)
            private-key-string (if (number? private-key)
                                 (util/hexify private-key)
                                 (get-mem [:keys :private-key]))
            public-key (if (number? private-key)
                         (->> private-key
                              (util/num->bytes 32)
                              es/get-pub-key
                              util/bytes->num)
                         nil)
            public-key-string (if (some? public-key)
                                (util/hexify public-key)
                                (get-mem [:keys :public-key]))]
        (when (some? public-key)
          (set-mem [:pubkey] public-key))
        (set-mem :keys {:private-key private-key-string
                        :public-key public-key-string
                        :name name
                        :about about
                        :picture picture
                        :nip05 nip05
                        :lud16 lud16})
        (data-storage/write-keys (get-mem :keys))
        (future (event-composers/compose-and-send-metadata-event)))

      (close-profile-frame (select (get-mem :frame) [:#profile-menu]) nil)
      (dispose! profile-frame))))

(defn make-data-panel [field-name content id editable?]
  (let [the-label (label :text field-name :size [150 :by 20])
        the-field (text :text content
                        :editable? editable?
                        :id id
                        :size [800 :by 20])]
    (left-right-split the-label the-field)))

(defn show-private-key [profile-frame _e]
  (let [private-key-field (select profile-frame [:#private-key-field])
        show-box (select profile-frame [:#show-box])
        show? (config show-box :selected?)]
    (config! private-key-field :text (if show?
                                       (get-mem [:keys :private-key])
                                       ""))))

(defn make-private-key-panel [profile-frame]
  (let [the-label (label :text "Private key:" :size [150 :by 20])
        the-field (text :text ""
                        :editable? true
                        :id :private-key-field
                        :size [800 :by 20])
        show-box (checkbox :text "show"
                           :id :show-box
                           :listen [:action (partial show-private-key profile-frame)])]

    (left-right-split the-label (horizontal-panel :items [the-field show-box]))))


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
        private-key-panel (make-private-key-panel profile-frame)
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
