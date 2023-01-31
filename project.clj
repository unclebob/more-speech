(defproject more-speech "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/data.json "2.4.0"]
                 [org.clojure/core.async "1.5.648"]
                 [clojure.java-time "0.3.3"]
                 [javax.xml.bind/jaxb-api "2.4.0-b180830.0359"]
                 [seesaw "1.5.0"]
                 [org.bouncycastle/bcprov-jdk18on "1.72"]
                 [com.xtdb/xtdb-core "1.23.0"]
                 [com.xtdb/xtdb-rocksdb "1.23.0"]]
  :profiles {:dev {:dependencies [[speclj "3.4.1"]]}
             :uberjar {:aot :all}}
  :repositories {"local" {:url "file:lib" :username "" :password ""}}
  :plugins [[speclj "3.4.1"]]
  :test-paths ["spec"]
  :main more-speech.core
  ;:java-cmd "/Users/unclebob/Library/Java/JavaVirtualMachines/openjdk-17.0.2/Contents/Home/bin/java"
  :java-source-paths ["java"]
;  :resource-paths ["jars/bcprov-jdk18on-171.jar"]
  ; remember to deploy: lein deploy local bcprov 1.7.1 jars/bcprov-jdk18on-171.jar
)

