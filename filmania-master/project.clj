(defproject filmania "0.1.0-SNAPSHOT"
  :description "Des films, du data, de la science... 
Du data-science pour les films..."
  :url "http://project.com/3I020"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
  [org.clojure/data.csv "0.1.3"]]
  :main ^:skip-aot filmania.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
  			:dev {:dependencies [[midje "1.8.3" :exclusions [org.clojure/clojure]]
                                  [org.clojure/tools.nrepl "0.2.12"]]
                   :plugins [[lein-midje "3.2.1"]]}
             :midje {}})
