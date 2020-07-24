(ns behave.engine.core
  (:require
   [behave.engine.initialstate :refer [make-initial-state-vector]]))

(defn run-test [model]
  (let [sv (make-initial-state-vector model)]
    sv))
