(ns behave.engine.initialstate
  (:require
   [clojure.algo.generic.functor :refer [fmap]])
  (:import
   [behave.ontology Model Variable Struct Field StateVector]))
(ns-unmap *ns* 'Process)
(import [behave.ontology Process])


(defn initial-state-dispatch-fn [item & _]
  (if (keyword? item)
    item
    (type item)))

(defmulti initial-state #'initial-state-dispatch-fn)

(defmethod initial-state Model [model]
  (fmap #(initial-state % model) (:processes model)))

(defmethod initial-state Process [process model]
  (fmap #(initial-state % model) (:variables process)))

(defn resolve-type-reference [ref model]
  (let [[type name] ref]
    (get (get model type) name)))

(defmethod initial-state Variable [variable model]
  (let [t (:type variable)]
    (if (seq? t)
      (initial-state (resolve-type-reference t model) model)
      (initial-state t model))))

(defmethod initial-state Struct [struct model]
  (fmap #(initial-state % model) (:fields struct)))

(defmethod initial-state Field [field model]
  (let [t (:type field)]
    (if (seq? t)
      (initial-state (resolve-type-reference))
      (initial-state t))))

(defmethod initial-state :int [& _]
  0)

(defmethod initial-state :string [& _]
  "")

(defmethod initial-state :bool [& _]
  false)

(defn make-initial-state-vector [model]
  (initial-state model))
