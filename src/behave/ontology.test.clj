(ns behave.ontology.test
  (:require [behave.ontology :refer :all]
            [clojure.test :as t :refer [deftest is testing run-tests]]))

(deftest test-ensure-map
  (let [par-a (->Parameter 'a :int)
        par-b (->Parameter 'b :int)
        var-a (->Variable 'a :int)]

    (testing "accepts single item"
      (is (equal?
           (sorted-map 'a par-a)
           (ensure-map par-a))))

    (testing "accepts a list of multiple items"
      (is (equal?
           (sorted-map 'a par-a 'b par-b)
           (ensure-map [par-a par-b]))))

    (testing "raises on a name collision"
      (is (thrown-with-msg?
           Exception #"Name conflict"
           (ensure-map [par-a var-a]))))))

(deftest test-make-structp
  (let [a-field (->Field 'aField 'int)
        another-field (->Field 'anotherField 'int)]
    (testing "accepts a single field"
      (is (equal? (->Struct 'foo {'aField a-field})
                  (make-struct 'foo :fields a-field))))
    (testing "accepts a list of fields"
      (is (equal? (->Struct 'foo {'aField a-field 'anotherField another-field})
                  (make-struct 'foo :fields [a-field another-field]))))
    (testing "accepts a map of fields"
      (is (equal? (->Struct 'foo {'aField a-field})
                  (make-struct 'foo :fields {'aField a-field}))))))

(deftest test-make-process
  (let [a-parameter-named-a (->Parameter 'a 'int)
        a-variable-named-a (->Variable 'a 'int )]
    (testing "raises if parameter and variable have the same name"
      (is (thrown-with-msg?
           Exception #"Parameter is shadowed by a variable"
           (make-process 'foo :parameters [a-parameter-named-a] :variables [a-variable-named-a]))))))
