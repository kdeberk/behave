(ns behave.dsl.parser
  (:require [instaparse.core :as insta]
            [instaparse.failure :as instafail]))

(def parser
  (insta/parser (slurp "src/behave/dsl/dsl.bnf")
                :auto-whitespace :standard
                :input-format :ebnf))

(defn remove-comments [string]
  (clojure.string/replace string #"\s*;;.*" ""))

(defmacro raise-if-insta-failure? [expr]
  `(let [tree# ~expr]
     (if (insta/failure? tree#)
       (throw (Exception. (instafail/pprint-failure tree#)))
       tree#)))

(defn parse
  ([string]      (raise-if-insta-failure? (insta/parse parser (remove-comments string))))
  ([string rule] (raise-if-insta-failure? (parser (remove-comments string) :start rule))))
