(ns more-speech.ui.formatters
  (:require [clojure.string :as string]
            [more-speech.nostr.util :as util :refer [hexify]]
            [more-speech.mem :refer :all]
            [more-speech.nostr.events :as events]
            [more-speech.nostr.contact-list :as contact-list]
            [more-speech.ui.formatter-util :refer :all]
            [more-speech.config :as config :refer [get-db]]
            [more-speech.db.gateway :as gateway])
  )

(defn format-user-id
  ([user-id]
   (format-user-id user-id 20))

  ([user-id length]
   (if (nil? user-id)
     ""
     (let [trusted? (contact-list/is-trusted? user-id)
           trusted-by (contact-list/which-contact-trusts user-id)
           petname (contact-list/get-petname user-id)
           id-string (abbreviate (util/num32->hex-string user-id) 10)
           profile (gateway/get-profile (get-db) user-id)
           profile-name (get profile :name id-string)
           ]
       (cond
         (seq petname)
         (abbreviate petname length)

         trusted?
         (abbreviate profile-name length)

         (some? trusted-by)
         (let [trusted-id-string (abbreviate (util/num32->hex-string trusted-by) 10)
               trusted-profile (gateway/get-profile (get-db) trusted-by)
               trusted-profile-name (get trusted-profile :name trusted-id-string)
               trusted-pet-name (contact-list/get-petname trusted-by)
               trusted-name (if (seq trusted-pet-name) trusted-pet-name trusted-profile-name)]
           (abbreviate (str profile-name "<-" trusted-name) length))

         :else
         (str "(" (abbreviate profile-name (- length 2)) ")"))))))

(defn name-exists? [name]
  (if (empty? name) nil name))

(defn get-best-name [id]
  (or (name-exists? (contact-list/get-petname id))
      (name-exists? (:name (gateway/get-profile (get-db) id)))
      (hexify id)))

(defn lookup-reference [event reference]
  (let [ref-string (re-find #"\d+" reference)
        index (Integer/parseInt ref-string)
        tags (:tags event)]
    (if (>= index (count tags))
      reference
      (try
        (let [id-string (-> tags (nth index) second)
              id (util/hex-string->num id-string)
              name (get-best-name id)]
          (str "@" name))
        (catch Exception e
          (prn `lookup-reference 'bad-id index tags)
          (prn (.getMessage e))
          "@-unknown-")))))

(defn replace-references [event]
  (let [padded-content (str " " (:content event) " ")
        references (re-seq config/reference-pattern padded-content)
        segments (string/split padded-content config/reference-pattern)
        referents (mapv (partial lookup-reference event) references)
        referents (conj referents " ")]
    (string/trim (apply str (interleave segments referents)))))

(defn get-subject [tags]
  (if (empty? tags)
    nil
    (let [tag (first tags)]
      (if (= (first tag) :subject)
        (abbreviate (second tag) 90)
        (recur (rest tags))))))

(defn format-header [{:keys [pubkey created-at tags] :as event}]
  (if (nil? event)
    "nil"
    (let [content (replace-references event)
          name (format-user-id pubkey)
          time (format-time created-at)
          subject (get-subject tags)
          [reply-id _ _] (events/get-references event)
          reply-mark (if (some? reply-id) "^" " ")
          dm-mark (if (= 4 (:kind event)) "🚫 " "")
          reaction-mark (if (some? (:reactions event)) "🤙" " ")
          header-text (-> content (string/replace \newline \~) (abbreviate 130))
          content (if (empty? subject)
                    header-text
                    (abbreviate (str subject "|" header-text) 130))]
      (format "%s%s %20s %s %s%s\n" reply-mark reaction-mark name time dm-mark content))))

(defn format-reply [event]
  (let [content (replace-references event)
        content (prepend> content)
        header (format ">From: %s at %s on %s\n"
                       (format-user-id (:pubkey event))
                       (format-time (:created-at event))
                       (first (:relays event))
                       )
        dm-prefix (if (:dm event)
                    (str "D @" (get-best-name (:pubkey event)) "\n")
                    "")]
    (str dm-prefix header ">---------------\n" content)))

(defn html-escape [content]
  (string/escape content {\& "&amp;"
                          \< "&lt;"
                          \> "&gt;"
                          \" "&quot;"
                          \' "&#x27;"
                          \/ "&#x2F;"}))

(defn break-newlines [content]
  (string/replace content "\n" "<br>"))

(defn non-breaking-spaces [s]
  (let [space-index (.indexOf s "  ")]
    (if (neg? space-index)
      s
      (str (subs s 0 space-index)
           "&nbsp"
           (non-breaking-spaces (subs s (+ space-index 1)))))))

(defn format-replies [content]
  (string/replace content " >" "\n>"))

(defn linkify [url]
  (str "<a href=\"" url "\">" url "</a>"))

(defn segment-text-url [content]
  (let [url (re-find config/url-pattern content)]
    (cond
      (not (nil? url))
      (let [url-start-index (string/index-of content url)
            url-end-index (+ url-start-index (.length url))
            text-sub (subs content 0 url-start-index)
            url-sub (subs content url-start-index url-end-index)
            rest (subs content url-end-index)]
        (concat
          (if (empty? text-sub)
            [[:url url-sub]]
            [[:text text-sub] [:url url-sub]])
          (segment-text-url rest)))
      (not (empty? content)) (list [:text content])
      :else '())))

(defn reformat-article [article]
  (let [segments (segment-text-url article)]
    (reduce
      (fn [formatted-content [seg-type seg]]
        (cond
          (= seg-type :text)
          (str formatted-content
               ((comp
                  non-breaking-spaces
                  break-newlines
                  html-escape
                  format-replies
                  ) seg)
               )
          (= seg-type :url)
          (str formatted-content (linkify seg))))
      ""
      segments)))

(defn hexify-event [event]
  (assoc event :pubkey (hexify (:pubkey event))
               :id (hexify (:id event))
               :sig (hexify (:sig event))))
