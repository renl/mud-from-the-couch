(defproject mud-from-the-couch "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [commons-net/commons-net "2.0"]
                 [clojure-lanterna "0.9.4"]
                 [org.clojure/core.async "0.2.385"]
                 [jinput "0.0.3"]
                 [seesaw "1.4.5"]
                 ]
  :main ^:skip-aot mud-from-the-couch.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
