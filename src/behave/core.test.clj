(ns behave.dsl.test
  (:require [clojure.test :refer [deftest is testing run-all-tests]]
            behave.dsl.rewriter.test
            behave.dsl.resolver.test))

(defmethod clojure.test/assert-expr 'equal? [msg form]
  (let [expected (nth form 1)
        expr (nth form 2)]
    `(try (let [actual# ~expr]
            (if (= ~expected actual#)
              (t/do-report {:type :pass, :message ~msg,
                            :expected ~expected, :actual actual#})
              (t/do-report {:type :fail, :message ~msg,
                            :expected ~expected, :actual actual#})))
          (catch Throwable e#
            (t/do-report {:type :fail, :message ~msg,
                          :expected nil, :actual e#})
            e#))))

(defn run-tests []
  (run-all-tests #"behave\.dsl\..*"))
