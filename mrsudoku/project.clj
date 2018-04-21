(defproject mrsudoku "0.2.0-SNAPSHOT"
  :description "Mini-projet Sudoku"
  :url ".."
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [midje "1.9.1"]
                 [seesaw "1.4.5"]
                 [org.clojure/core.match "0.3.0-alpha5"]]
  :main ^:skip-aot mrsudoku.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
            :dev {:dependencies [[midje "1.8.3" :exclusions [org.clojure/clojure]]
                                  [org.clojure/tools.nrepl "0.2.12"]]
                   :plugins [[lein-midje "3.2.1"]]}
             :midje {}})
