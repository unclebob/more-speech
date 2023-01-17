(ns more-speech.config)

(def default-font "COURIER-PLAIN-14")
(def bold-font "COURIER-BOLD-14")
(def small-font "COURIER_PLAIN-9")

(def article-width 120)

(def days-to-read 7) ;how many daily message files to read in.
(def read-contact-lists-days-ago 2)

(def migration-level 9)
(def version "202212091526")

(def test-run? false)
;---configuration files
(def private-directory (atom "private"))
(def migration-filename (atom "private/migration"))
(def nicknames-filename (atom "private/nicknames")) ;grandfathered.
(def profiles-filename (atom "private/profiles"))
(def keys-filename (atom "private/keys"))
(def relays-filename (atom "private/relays"))
(def read-event-ids-filename (atom "private/read-event-ids"))
(def tabs-filename (atom "private/tabs")) ;grandfathered.
(def tabs-list-filename (atom "private/tabs-list"))
(def messages-directory (atom "private/messages"))
(def messages-filename (atom "private/messages/message-file"))
(def user-configuration-filename (atom "private/user-configuration"))
(def contact-lists-filename (atom "private/contact-lists"))

(def user-name-pattern #"\@[\w\-]+")
(def pubkey-pattern #"[0-9a-f]{64}+")
(def user-name-chars #"[\w\-]+")
(def reference-pattern #"\#\[\d+\]")

(def proof-of-work-default 16)

;; https://daringfireball.net/2010/07/improved_regex_for_matching_urls
(def url-pattern #"(?i)\b(?:(?:[a-z][\w-]+:(?:/{1,3}|[a-z0-9%])|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(?:(?:[^\s()<>]+|(?:\(?:[^\s()<>]+\)))*\))+(?:\(?:(?:[^\s()<>]+|(?:\(?:[^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'\".,<>?«»“”‘’]))")


