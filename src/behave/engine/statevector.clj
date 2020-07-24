(ns behave.engine.statevector
  (:require
   [clojure.algo.generic.functor :refer [fmap]])
  (:import
   [behave.ontology Model Variable Struct Field]))
(ns-unmap *ns* 'Process)
(import [behave.ontology Process])


(defn constraint-holds [state expr model]
  true)

(defn update-state [state expr model]
  expr)
