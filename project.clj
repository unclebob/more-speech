(defproject more-speech "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/data.json "2.4.0"]
                 [org.clojure/core.async "1.5.648"]
                 [clojure.java-time "0.3.3"]
                 [seesaw "1.5.0"]
                 [dev.dirs/directories "26"]
                 ]
  :profiles {:dev {:dependencies [[speclj "3.4.1"]]}}
  :plugins [[speclj "3.4.1"]]
  :test-paths ["spec"]
  :main more-speech.core
  ;:java-cmd "/Users/unclebob/Library/Java/JavaVirtualMachines/openjdk-17.0.2/Contents/Home/bin/java"
  :java-source-paths ["src"])
