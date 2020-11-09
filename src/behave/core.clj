(ns behave.core
  (:require behave.ontology
            [behave.dsl.core :refer [parse-model]]
            behave.engine.core))

(defn run-test [name]
  (let [model (parse-model name)]
    (behave.engine.core/run-test model)))


;; TODO:

;; DSL
;; - domain type
;; - negation constraints for alternative paths in if statements
;; - pruning graph

;; Graphics
;; - generating dot image from transitons

;; Testing
;; - determine first label to be received
;; - determine unspecified values for this label
;; - send it to the adapter
