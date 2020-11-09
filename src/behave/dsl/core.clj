(ns behave.dsl.core
  (:require [behave.dsl.parser :refer [parse]]
            [behave.dsl.rewriter :refer [rewrite]]
            [behave.dsl.resolver :refer [resolve-references]]
            [behave.dsl.graph :refer [generate-graph]]))

(defn parse-model [name]
  (let [text (slurp (format "models/%s.model" name))
        tree (parse text)]
    (generate-graph (resolve-references (rewrite tree)))))
