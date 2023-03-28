(ns more-speech.logger.default-spec
  (:require [speclj.core :refer :all]
            [more-speech.logger.default :refer :all]))

(describe "Default Logger"
  (with-stubs)
  (before (set-level 1))

  (it "logs a message"
    (with-redefs [printf (stub :printf)
                  get-now (stub :get-now {:return 0})]
      (log 1 "message")
      (await log-agent)
      (should-have-invoked :printf {:with [:* 1 "12/31/69 18:00:00" "message"]})))

  (it "Does not log a message more detailed message"
      (with-redefs [printf (stub :printf)
                    get-now (stub :get-now {:return 0})]
        (log 2 "message")
        (await log-agent)
        (should-not-have-invoked :printf)))

  (it "Does log a message more detailed message if the level is set"
      (with-redefs [printf (stub :printf)
                    get-now (stub :get-now {:return 0})]
        (set-level 2)
        (log 2 "message")
        (await log-agent)
        (should-have-invoked :printf {:with [:* 2 "12/31/69 18:00:00" "message"]})))
  )
