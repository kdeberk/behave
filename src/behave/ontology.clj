(ns behave.ontology
  (:require [behave.dsl.helpers :refer [type-from-value]]))

(ns-unmap *ns* 'Process)

(defn- map-by-name [col]
  (if (empty? col)
    {}
    (let [dup-names (filter (fn [e] (< 1 (val e)))
                            (frequencies (map :name col)))]
      (if (= 0 (count dup-names))
        (into (array-map) (map vector (map :name col) col))
        (throw (ex-info "Name conflict" {:names dup-names}))))))

(defn ensure-map [thing]
  (cond
    (record? thing) (array-map (:name thing) thing)
    (map? thing) thing
    (seqable? thing) (map-by-name thing)))

(defrecord Constant [name value type])

(defrecord Struct [name fields])
(defn make-struct [name & {:keys [fields]}]
  (let [fields (ensure-map fields)]
    (->Struct name fields)))

(defrecord Parameter [name type])
(defrecord Variable [name type])
(defrecord Field [name type])

(defrecord Function [name return-type parameters expr])
(defn make-function [name return-type parameters expr]
  (let [parameters (ensure-map parameters)]
    (->Function name return-type parameters expr)))

(defrecord Stimulus [name type])

(defrecord Response [name parameters])
(defn make-response [name parameters]
  (let [parameters (ensure-map parameters)]
    (->Response name parameters)))

(defrecord State [name])
(defrecord Transition [from to constraint update label spawn])

(defn make-transition [from to & {:keys [constraint update label spawn]}]
  (->Transition from to constraint update label spawn))

(defrecord Process [name parameters variables body init-state states transitions])
(defn make-process [name & {:keys [parameters variables body init-state states transitions]}]
  (let [parameters (ensure-map parameters)
        variables (ensure-map variables)
        states (ensure-map states)
        transitions (or transitions [])]
    (let [shared-names (clojure.set/intersection (set (keys parameters))
                                                 (set (keys variables)))]
      (when-let [name (first shared-names)]
        (throw (ex-info "Parameter is shadowed by a variable"
                        {:parameters (name parameters) :variables (name variables)}))))
    (->Process name parameters variables body init-state states transitions)))

(defrecord Model [name constants structs functions stimuli responses processes])
(defn make-model [& {:keys [name constants structs functions stimuli responses processes]}]
  (let [constants (ensure-map constants)
        structs (ensure-map structs)
        functions (ensure-map functions)
        stimuli (ensure-map stimuli)
        responses (ensure-map responses)
        processes (ensure-map processes)]
    (->Model name constants structs functions stimuli responses processes)))

(defrecord StateVector [variables])
