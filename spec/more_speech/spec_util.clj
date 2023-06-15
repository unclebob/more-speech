(ns more-speech.spec-util
  (:require
    [more-speech.config :as config]
    [more-speech.db.in-memory :as in-memory]
    [more-speech.mem :as mem]
    [speclj.core :refer :all]))

(defmacro setup-db-mem []
  `(list (with ~'db (in-memory/get-db))
         (before-all (config/set-db! :in-memory))
         (before (in-memory/clear-db @~'db)
                 (mem/clear-mem))))

;the list forces the execution of the three statements.
;(prn (macroexpand '(setup-db-mem)))