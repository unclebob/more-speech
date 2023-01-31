(ns more-speech.ui.swing.main-window-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.swing.main-window :as mw]
            [more-speech.db.gateway :as gateway]
            [more-speech.db.in-memory :as in-memory]
            [more-speech.config :as config]))

(declare db)

(describe "Main window"
  (with db (in-memory/get-db))
  (before-all (config/set-db! :in-memory))
  (it "makes a profile line"
    (gateway/add-profile @db 1 {:name "name" :picture "picture"})
    (should= "name                 0000000000000000000000000000000000000000000000000000000000000001 picture"
             (mw/make-profile-line 1))))
