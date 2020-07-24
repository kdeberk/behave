(ns behave.engine.updatestate
  (:require
   [clojure.algo.generic.functor :refer [fmap]])
  (:import
   [behave.ontology StateVector]))

(defn do-update [sv update model]
  ;; TODO: check if update is set
  ;; if so, get the variable
  ;; set the bindings and execute the expression
  )
