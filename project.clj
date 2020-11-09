(defproject behave "0.1.0"
  :description "Framwork for modeling and testing system behavior"
  :url "gitlab.messagebird.io/kevin.deberk/behave"
  :main ^:skip-aot behave.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}

  :dependencies [[org.clojure/clojure "1.10.1"]
                 ;; For parsing the DSL
                 [instaparse "1.4.10"]
                 ;; For macrolet
                 [org.clojure/tools.macro "0.1.2"]
                 ;; For fmap
                 [org.clojure/algo.generic "0.1.3"]]

  :source-paths ["src" "src/behave/" "src/behave/dsl"])
