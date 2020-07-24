(ns behave.dsl.graph
  (:require
   [behave.ontology :refer :all]
   [clojure.algo.generic.functor :refer [fmap]])
  (:import
   [behave.ontology Model Struct Function Stimulus Response Field Parameter Variable Constant]))
(ns-unmap *ns* 'Process)
(import [behave.ontology Process])

(defn generate-graph-dispatch-fn [item & _]
  (if (seq? item)
    (first item)
    (type item)))

(defmulti generate-graph #'generate-graph-dispatch-fn)

(defmethod generate-graph Model [model]
  (let [processes (:processes model)]
    (assoc model :processes (fmap generate-graph processes))))

(defmethod generate-graph Process [process]
  (let [state-generator (let [counter (atom 0)]
                          (fn []
                            (do (swap! counter inc)
                                (->State @counter))))
        init-state (state-generator)
        [_ transitions] (generate-graph (:body process) init-state state-generator)]
    (assoc process
           :init-state init-state
           :transitions transitions)))

(defmethod generate-graph :block [[_ & items] cur-state gen]
  (reduce (fn [[s ts] item]
            (let [[s ts'] (generate-graph item s gen)]
              [s (concat ts ts')]))
          [cur-state []]
          items))

(defmethod generate-graph :choice [[_ & options] cur-state gen]
  (let [start (gen)
        end (gen)]
    [end (reduce (fn [ts option]
                   (let [[s' ts'] (generate-graph option start gen)]
                     (concat ts ts' (when s'
                                      [(->InnerTransition s' end)]))))
                 [(->InnerTransition cur-state start)]
                 options)]))

(defmethod generate-graph :state [[_ state] cur-state gen]
  [state []])

(defmethod generate-graph :receive [[_ var stimulus] cur-state gen]
  (let [to (gen)]
    (if var
      [to [(make-transition cur-state to :label stimulus :update [var stimulus])]]
      [to [(make-transition cur-state to :label stimulus)]])))

(defmethod generate-graph :send [[_ response constraint] cur-state gen]
  (let [to (gen)]
    [to [(make-transition cur-state to :label response :constraint constraint)]]))

(defmethod generate-graph :constraint [[_ constraint] cur-state gen]
  (let [to (gen)]
    [to [(make-transition cur-state to :constraint constraint)]]))

(defmethod generate-graph :update [[_ update expr] cur-state gen]
  (let [to (gen)]
    [to [(make-transition cur-state to :update [update expr])]]))

(defmethod generate-graph :spawn [[_ process & args] cur-state gen]
  (let [to (gen)]
    [to [(make-transition cur-state to :spawn process :constraint args)]]))

(defmethod generate-graph :goto [[_ state] cur-state gen]
  [nil [(make-transition cur-state state)]])
