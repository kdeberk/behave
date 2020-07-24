(ns behave.dsl.helpers
  (:require
   [clojure.pprint :refer [cl-format]])
  (:import [behave.dsl ParserException]))

(defmacro parser-error [fmt & params]
  `(throw (ParserException. (cl-format nil ~fmt ~@params))))

(defn type-from-value [value]
  (cond (int? value) :int
        (string? value) :string
        (or (= "true" value)
            (= "false" value)) :bool
        :else (parser-error "Uknown type for literal '%s'" value)))

(defn third [seq]
  (nth seq 2))
