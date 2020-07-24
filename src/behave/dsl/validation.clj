(ns behave.dsl.validation
  (:require
   [behave.ontology :refer :all]
   [behave.dsl.helpers :refer [parser-error]]
   [clojure.algo.generic.functor :refer [fmap]])
  (:import
   [behave.ontology Model Struct Function]))
(ns-unmap *ns* 'Process)
(import [behave.ontology Process])

(defn validate-dispatch-fn [item & _]
  (if (seq? item)
    (first item)
    (type item)))

(defmulti validate #'validate-dispatch-fn)


;; TODO: structs may not be (in)directly recursive, perhaps move that test to validation
