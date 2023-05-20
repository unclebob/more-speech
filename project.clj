(defproject more-speech "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/data.json "2.4.0"]
                 [org.clojure/core.async "1.6.673"]
                 [clojure.java-time "1.2.0"]
                 [javax.xml.bind/jaxb-api "2.4.0-b180830.0359"]
                 [seesaw "1.5.0"]
                 [org.bouncycastle/bcprov-jdk18on "1.72"]
                 [com.xtdb/xtdb-core "1.23.1"]
                 [com.xtdb/xtdb-rocksdb "1.23.1"]
                 [org.slf4j/slf4j-nop "1.7.30"] ;1.7.30 works.
                 [clj-http "3.12.3"]
                 [org.bovinegenius/exploding-fish "0.3.6"]
                 [org.clojure/test.check "1.1.1"]]
  :profiles {:dev {:dependencies [[speclj "3.4.3"]]}
             :uberjar {:aot :all}}
  :repositories {"local" {:url "file:lib" :username "" :password ""}}
  :plugins [[speclj "3.4.3"]]
  :test-paths ["spec"]
  :main more-speech.core
  :java-source-paths ["java"]
)

