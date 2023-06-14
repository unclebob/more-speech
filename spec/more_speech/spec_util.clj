(ns more-speech.spec-util)

(defmacro setup-db-mem []
  '(list (with db (more-speech.db.in-memory/get-db))
         (before-all (more-speech.config/set-db! :in-memory))
         (before (more-speech.db.in-memory/clear-db @db)
                 (more-speech.mem/clear-mem))))