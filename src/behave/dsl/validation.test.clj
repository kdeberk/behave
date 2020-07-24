(ns behave.dsl.validation.test
  (:require
   [behave.dsl.validation :refer :all]
   [behave.dsl.rewriter :as rewriter]
   [behave.dsl.parser :as parser]
   [behave.ontology :refer :all]
   [clojure.test :as t :refer [deftest is testing]])
  (:import
   [behave.dsl ParserException]))

(def empty-model
  (->Model {} {} {} {} {} {}))
(def filled-model
  (->Model {}
           {'aStruct (->Struct 'aStruct {})}
           {} {} {} {}))

(defmethod t/assert-expr 'not-throwing? [msg form]
  (let [body (rest form)]
    `(try ~@body
          (t/do-report {:type :pass, :message ~msg,
                        :expected nil, :actual nil})
          (catch Throwable e#
            (t/do-report {:type :fail, :message ~msg,
                          :expected nil, :actual e#})
            e#))))

(defn parse [text rule]
  (rewriter/rewrite (parser/parse text rule)))

(deftest validate-struct
  (testing "validate struct with int field"
    (is
     (not-throwing?
      (validate (parse "struct aStruct { aField int }" :struct-decl)
                filled-model))))
  (testing "validate struct with struct field"
    (is 
     (not-throwing?
      (validate (parse "struct someStruct { aField aStruct }" :struct-decl)
                filled-model))))
  (testing "validate struct with unknown field"
    (is
     (thrown-with-msg?
      ParserException #"Unknown type 'unknownType'"
      (validate (parse "struct someStruct { aField unknownType }" :struct-decl)
                empty-model)))))

(deftest validate-function
  ;; TODO: validate that function contains an expression that results in type
  (testing "validate function with simple parameter type"
    (is 
     (not-throwing?
      (validate (parse "func someFunc(a int) int { 42 }" :fn-decl)
                filled-model))))
  (testing "validate function with simple return type"
    (is
     (not-throwing?
      (validate (parse "func someFunc() bool { false }" :fn-decl)
                filled-model))))
  (testing "validate function with struct parameter type"
    (is
     (not-throwing?
      (validate (parse "func someFunc(a aStruct) int { 42 }" :fn-decl)
                filled-model))))
  (testing "validate function with struct return type"
    (is
     (not-throwing?
      (validate (parse "func someFunc(a aStruct) aStruct { a }" :fn-decl)
                filled-model))))
  (testing "validate function with unknown parameter type"
    (is
     (thrown-with-msg?
      ParserException #"Unknown type 'mysteryType'"
      (validate (parse "func someFunc(a mysteryType) int { 0 }" :fn-decl)
                empty-model))))
  (testing "validate function with unknown return type"
    (is
     (thrown-with-msg?
      ParserException #"Unknown type 'unknownType'"
      (validate (parse "func someFunc() unknownType { nil }" :fn-decl)
                empty-model)))))

(deftest validate-stimulus
  (testing "validate stimulus with simple return type"
    (is
     (not-throwing?
      (validate (parse "stimulus someStimulus int" :stimulus-decl)
                filled-model))))
  (testing "validate stimulus with struct return type"
    (is
     (not-throwing?
      (validate (parse "stimulus someStimulus aStruct" :stimulus-decl)
                filled-model))))
  (testing "validate stimulus with unknown return type"
    (is
     (thrown-with-msg?
      ParserException #"Unknown type 'unknownType'"
      (validate (parse "stimulus someStimulus unknownType" :stimulus-decl)
                filled-model)))))

(deftest validate-response
  (testing "validate response with simple parameter type"
    (is
     (not-throwing?
      (validate (parse "response someResponse(a int)" :response-decl)
                filled-model))))
  (testing "validate response with struct parameter type"
    (is
     (not-throwing?
      (validate (parse "response someResponse(a aStruct)" :response-decl)
                filled-model))))
  (testing "validate response with unknown parameter type"
    (is
     (thrown-with-msg?
      ParserException #"Unknown type 'mysteryType'"
      (validate (parse "response someResponse(a mysteryType)" :response-decl)
                empty-model)))))

(deftest validate-process
  (testing "validate process with simple parameter type"
    (is
     (not-throwing?
      (validate (parse "proc someProcess(a int) {}" :proc-decl)
                filled-model))))
  (testing "validate process with complex parameter type"
    (is
     (not-throwing?
      (validate (parse "proc someProcess(a aStruct) {}" :proc-decl)
                filled-model))))
  (testing "validate process with unknown parameter type"
    (is
     (thrown-with-msg?
      ParserException #"Unknown type 'unknownType'"
      (validate (parse "proc someProcess(a unknownType) {}" :proc-decl)
                empty-model))))
  (testing "validate process with simple variable type"
    (is
     (not-throwing?
      (validate (parse "proc someProcess() { var a int }" :proc-decl)
                filled-model))))
  (testing "validate process with complex variable type"
    (is
     (not-throwing?
      (validate (parse "proc someProcess() { var a aStruct }" :proc-decl)
                filled-model))))
  (testing "validate process with unknown variable type"
    (is
     (thrown-with-msg?
      ParserException #"Unknown type 'unknownType'"
      (validate (parse "proc someProcess() { var a unknownType }" :proc-decl)
                empty-model))))
  )

(defn run-tests []
  (binding [clojure.test/*stack-trace-depth* 10]
    (clojure.test/run-tests)))
