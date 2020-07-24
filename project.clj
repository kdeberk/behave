(defproject behave "0.1.0"
  :description "Framwork for modeling and testing system behavior"
  :url "gitlab.messagebird.io/kevin.deberk/behave"
  :main ^:skip-aot behave.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}

  ;; Settings for parsing and generating gRPC code
  :plugins [[lein-protoc "0.5.0"]]
  :protoc-version "3.12.0"
  :protoc-grpc {:version "1.30.0"}
  :proto-target-path "target/generated-sources/protobuf"
  :java-source-paths ["target/generated-sources/protobuf"]

  :dependencies [[org.clojure/clojure "1.10.1"]
                 ;; For protobuf & gRPC
                 [com.google.protobuf/protobuf-java "3.12.2"]
                 [javax.annotation/javax.annotation-api "1.2"]
                 [io.netty/netty-codec-http2 "4.1.25.Final"]
                 [io.grpc/grpc-core "1.30.2"
                  :exclusions [io.grpc/grpc-api]]
                 [io.grpc/grpc-netty "1.30.2"
                  :exclusions [io.grpc/grpc-core
                               io.netty/netty-codec-http2]]
                 [io.grpc/grpc-protobuf "1.30.2"]
                 [io.grpc/grpc-stub "1.30.2"]
                 ;; For unify-gensyms, not used atm
                 [potemkin "0.4.5"]
                 ;; For parsing the DSL
                 [instaparse "1.4.10"]
                 ;; For macrolet
                 [org.clojure/tools.macro "0.1.2"]
                 ;; For fmap
                 [org.clojure/algo.generic "0.1.3"]]

  :source-paths ["src" "src/behave/" "src/behave/dsl"])
