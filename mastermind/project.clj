(defproject mastermind "0.1.0-SNAPSHOT"
  :description "This is a project for the ue declaratif programming named mastermind"
  :url "http://project_3I020.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
  				[clj-http "2.0.0"]]
  :main ^:skip-aot mastermind.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
