(ns behave.dsl.resolver.test
  (:require
   [behave.dsl.resolver :refer [resolve-references]]
   [behave.dsl.rewriter :as rewriter]
   [behave.dsl.parser :as parser]
   [behave.ontology :refer :all]
   [clojure.test :as t :refer [deftest is testing run-tests]])
  (:import
   [behave.dsl ParserException]))
(load "/behave/test/helpers")

(defn make-resolver [rule & args]
  (fn [text]
    (apply
     resolve-references
     (cons (rewriter/rewrite (parser/parse text rule))
           args))))

;; (deftest test-resolve-model
;;   (let []))
;; TODO: fill with some code that contains all possible valid cross references

(deftest test-resolve-field
  (let [ns {:structs {'someStruct (make-struct 'someStruct)}
            :domains {'someDomain (->Domain 'someDomain :int [])}}]

    (testing "field with int type"
      (is (equal?
           (->Field 'aField :int)
           (resolve-references (->Field 'aField :int) ns))))

    (testing "field with struct type"
      (is (equal?
           (->Field 'aField '(:structs someStruct))
           (resolve-references (->Field 'aField 'someStruct) ns))))

    (testing "field with domain type"
      (is (equal?
           (->Field 'aField '(:domains someDomain))
           (resolve-references (->Field 'aField 'someDomain) ns))))
    
    (testing "field with unknown type"
      (is (thrown-with-msg?
           ParserException #"Could not resolve type unknownType"
           (str (resolve-references (->Field 'aField 'unknownType) ns)))))))


(deftest resolve-domain
  (let [ns {:constants {'someInt (->Constant 'someInt 4 :int)
                        'someOtherInt (->Constant 'someOtherInt 5 :int)
                        'someString (->Constant 'someString "foo" :string)}}
        resolve (make-resolver :domain-decl ns)]

    (testing "domain with only ints"
      (is (equal?
           (->Domain 'someDomain :int [1 2 3])
           (resolve "type someDomain domain int [1, 2, 3]"))))

    (testing "domain with conflicting literal values"
      (is (thrown-with-msg?
           ParserException #"Type mismatch: Domain with value :int contains values with types: :string, :bool"
           (str (resolve "type someDomain domain int [1, 'foo', false]")))))

    (testing "domain with constants"
      (is (equal?
           (->Domain 'someDomain :int '[(:constants someInt) (:constants someOtherInt)])
           (resolve "type someDomain domain int [someInt, someOtherInt]"))))

    (testing "domain with literals and constants"
      (is (equal?
           (->Domain 'someDomain :int '[1 (:constants someInt) 7 (:constants someOtherInt)])
           (resolve "type someDomain domain int [1, someInt, 7, someOtherInt]"))))

    (testing "domain with type mismatching constants"
      (is (thrown-with-msg?
           ParserException #"Type mismatch: Domain with value :int contains values with types: :string"
           (resolve "type someDomain domain int [someInt, someString]"))))

    (testing "domain with type mismatching literals and constants"
      (is (thrown-with-msg?
           ParserException #"Type mismatch: Domain with value :int contains values with types: :string"
           (resolve "type someDomain domain int [1, someString]"))))

    (testing "domain with unknown constant reference"
      (is (thrown-with-msg?
           ParserException #"Could not resolve symbol unknownConstant"
           (resolve "type someDomain domain int [unknownConstant]"))))))


(deftest test-resolve-struct
  (let [ns {:structs {'someStruct (make-struct 'someStruct)}}
        resolve (make-resolver :struct-decl ns)]
    (testing "struct with int field"
      (is (equal?
           (make-struct 'someStruct :fields [(->Field 'aField :int)])
           (resolve "type someStruct struct { aField int }"))))

    (testing "struct with struct field"
      (is (equal?
           (make-struct 'someStruct :fields (->Field 'aField '(:structs someStruct)))
           (resolve "type someStruct struct { aField someStruct }"))))

    (testing "struct with unknown field"
      (is (thrown-with-msg?
           ParserException #"Could not resolve type unknownType"
           (str (resolve "type someStruct struct { aField unknownType }")))))))


(deftest test-resolve-function
  (let [ns {:constants {'someConstant (->Constant 'someConstant 42 :int)}
            :structs {'someStruct (make-struct 'someStruct)}}
        resolve (make-resolver :fn-decl ns)]

    (testing "parameters"

      (testing "function with simple parameter type"
        (is (equal?
             (make-function 'someFunc :int [(->Parameter 'a :int)] 42)
             (resolve "func someFunc(a int) int { 42 }"))))

      (testing "function with struct parameter type"
        (is (equal?
             (make-function 'someFunc :int [(->Parameter 'a '(:structs someStruct))] 42)
             (resolve "func someFunc(a someStruct) int { 42 }"))))

      (testing "function with unknown parameter type"
        (is (thrown-with-msg?
             ParserException #"Could not resolve type mysteryType"
             (str (resolve "func someFunc(a mysteryType) int { 0 }"))))))

    (testing "return"

      (testing "function with simple return type"
        (is (equal?
             (make-function 'someFunc :bool [] false)
             (resolve "func someFunc() bool { false }"))))

      (testing "function with struct return type"
        (is (equal?
             (make-function 'someFunc '(:structs someStruct) [(->Parameter 'a '(:structs someStruct))] '(:parameters a))
             (resolve "func someFunc(a someStruct) someStruct { a }"))))

      (testing "function with unknown return type"
        (is (thrown-with-msg?
             ParserException #"Could not resolve type unknownType"
             (str (resolve "func someFunc() unknownType { 42 }"))))))

    (testing "expression"

      (testing "function with parameter reference in expression"
        (is (equal?
             (make-function 'someFunc :int [(->Parameter 'a :int)] `(~'+ (:parameters ~'a) 1))
             (resolve "func someFunc(a int) int { a + 1 }"))))

      (testing "function with constant reference in expression"
        (is (equal?
             (make-function 'someFunc :int [(->Parameter 'a :int)] `(~'+ (:parameters ~'a) (:constants ~'someConstant)))
             (resolve "func someFunc(a int) int { a + someConstant }"))))

      (testing "function with invalid reference in expression"
        (is (thrown-with-msg?
             ParserException #"Could not resolve symbol b"
             (str (resolve "func someFunc(a int) int { b + 1 }"))))))

      (testing "function with invalid reference in expression"
        (is (thrown-with-msg?
             ParserException #"Function is declared to return type :bool but expression resolves to type :int"
             (str (resolve "func someFunc(a int) bool { a + 1 }")))))))


(deftest test-resolve-stimulus
  (let [ns {:structs {'someStruct (make-struct 'someStruct)}}
        resolve (make-resolver :stimulus-decl ns)]

    (testing "stimulus with simple return type"
      (is (equal?
           (->Stimulus 'someStimulus :int)
           (resolve "stimulus someStimulus int"))))

    (testing "stimulus with struct return type"
      (is (equal?
           (->Stimulus 'someStimulus '(:structs someStruct))
           (resolve "stimulus someStimulus someStruct"))))

    (testing "stimulus with nil return type"
      (is (equal?
           (->Stimulus 'someStimulus nil)
           (resolve "stimulus someStimulus {}"))))

    (testing "stimulus with unknown return type"
      (is (thrown-with-msg?
           ParserException #"Could not resolve type unknownType"
           (str (resolve "stimulus someStimulus unknownType")))))))


(deftest test-resolve-response
  (let [ns {:structs {'someStruct (make-struct 'someStruct)}}
        resolve (make-resolver :response-decl ns)]

    (testing "response with simple parameter type"
      (is (equal?
           (make-response 'someResponse [(->Parameter 'a :int)])
           (resolve "response someResponse(a int)"))))

    (testing "response with struct parameter type"
      (is (equal?
           (make-response 'someResponse [(->Parameter 'a '(:structs someStruct))])
           (resolve "response someResponse(a someStruct)"))))

    (testing "response with unknown parameter type"
      (is (thrown-with-msg?
           ParserException #"Could not resolve type mysteryType"
           (str (resolve "response someResponse(a mysteryType)")))))))


(deftest test-resolve-process
  (let [ns {:structs {'someStruct (make-struct 'someStruct)}}
        resolve (make-resolver :proc-decl ns)]

    (testing "parameter"

      (testing "process with simple parameter type"
        (is (equal?
             (make-process 'someProcess
                           :parameters [(->Parameter 'a :int)]
                           :body '(:block))
             (resolve "proc someProcess(a int) {}"))))

      (testing "process with struct parameter type"
        (is (equal?
             (make-process 'someProcess
                           :parameters [(->Parameter 'a '(:structs someStruct))]
                           :body '(:block))
             (resolve "proc someProcess(a someStruct) {}"))))

      (testing "process with unknown parameter type"
        (is (thrown-with-msg?
             ParserException #"Could not resolve type unknownType"
             (str (resolve "proc someProcess(a unknownType) {}"))))))

    (testing "variable"

      (testing "process with simple variable type"
        (is (equal?
             (make-process 'someProcess
                           :variables [(->Variable 'a :int)]
                           :body '(:block))
             (resolve "proc someProcess() { var a int }"))))

      (testing "process with struct variable type"
        (is (equal?
             (make-process 'someProcess
                           :variables [(->Variable 'a '(:structs someStruct))]
                           :body '(:block))
             (resolve "proc someProcess() { var a someStruct }"))))

      (testing "process with unknown variable type"
        (is (thrown-with-msg?
             ParserException #"Could not resolve type unknownType"
             (str (resolve "proc someProcess() { var a unknownType }"))))))

    (testing "process with a body"
      (is (equal? (make-process 'someProcess
                                :body `(:block (:state ~(->State 'a))
                                               (:goto ~(->State 'a)))
                                :init-state (->State 'a)
                                :states [(->State 'a)])
                  (resolve "proc someProcess() { a: goto a }"))))))


(deftest test-resolve-goto
  (let [ns {:states {'a 'a}}
        resolve (make-resolver :proc-goto-stmt ns)]

    (testing "goto to known state"
      (is (equal?
           `(:goto ~'a)
           (resolve "goto a"))))

    (testing "goto to unknown state"
      (is (thrown-with-msg?
           ParserException #"State b is not defined"
           (str (resolve "goto b")))))))


(deftest test-resolve-spawn
  (let [ns {:variables {'a (->Variable 'a :int)}
            :processes {'someProcess (make-process 'someProcess)
                        'anotherProcess (make-process 'anotherProcess :parameters [(->Parameter 'b :int)])}}
        resolve (make-resolver :proc-spawn-stmt ns)]

    (testing "spawn with known process"
      (is (equal? `(:spawn ~'someProcess)
                  (resolve "spawn someProcess()"))))

    (testing "spawn with unknown process"
      (is (thrown-with-msg?
           ParserException #"Unknown process named unknownProcess"
           (str (resolve "spawn unknownProcess()")))))

    (testing "spawn with known parameters"
      (is (equal?
           `(:spawn ~'anotherProcess (:variables ~'a))
           (resolve "spawn anotherProcess(a)"))))))


(deftest test-resolve-expr
  (let [ns {:constants {'someConst (->Constant 'someConst 42 :int)}
            :variables {'a (->Variable 'a :int) 's (->Variable 's '(:structs outerStruct))}
            :parameters {'b (->Parameter 'b :int)}
            :structs {'innerStruct (make-struct 'innerStruct :fields [(->Field 'i :int)])
                      'outerStruct (make-struct 'outerStruct :fields [(->Field 'inner '(:structs innerStruct))])}
            :functions {'someFunc (make-function 'someFunc :int [(->Parameter 'a :int) (->Parameter 'b :int)] 2)}}
        resolve (make-resolver :expr-start ns)]

    (testing "opcalls"

      (testing "addition"
        (is (equal?
             `((~'+ (:variables ~'a) (:parameters ~'b)) :int)
             (resolve "a + b"))))

      (testing "addition with wrong types"
        (is (thrown-with-msg?
             ParserException #"\+ is not implemented for types \(:int :bool\)"
             (resolve "a + true"))))

      (testing "less than comparison"
        (is (equal?
             `((~'< (:variables ~'a) (:parameters ~'b)) :bool)
             (resolve "a < b"))))

      (testing "less than comparison with wrong types"
        (is (thrown-with-msg?
             ParserException #"< is not implemented for types \(:int :bool\)"
             (resolve "a < true"))))

      (testing "equality comparison"
        (is (equal?
             `((~'= (:variables ~'a) (:parameters ~'b)) :bool)
             (resolve "a == b"))))

      (testing "equality comparison with wrong types"
        (is (thrown-with-msg?
             ParserException #"= is not implemented for types \(:int :bool\)"
             (resolve "a == true"))))

      (testing "in check"
        (is (equal?
             `((~'in (:variables ~'a) [1 2 (:constants ~'someConst)]) :bool)
             (resolve "a in [1, 2, someConst]"))))

      (testing "in check with mismatching types"
        (is (thrown-with-msg?
             ParserException #"Type mismatch: :bool cannot be in \[:int\]"
             (resolve "true in [1, 2, someConst]")))))

    (testing "funcalls"

      (testing "function call with known parameters"
        (is (equal?
             `(((:fn ~'someFunc) (:variables ~'a) (:parameters ~'b)) :int)
             (resolve "someFunc(a, b)"))))

      (testing "unknown function call"
        (is (thrown-with-msg?
             ParserException #"Could not find function named unknownFunc"
             (str (resolve "unknownFunc(a, b)"))))))

    (testing "references"

      (testing "reference to constant"
        (is (equal?
             `((~'+ 1 (:constants ~'someConst)) :int)
             (resolve "1 + someConst"))))

      (testing "to known parameter"
        (is (equal?
             `((:variables ~'a) :int)
             (resolve "a"))))

      (testing "unknown parameters"
        (is (thrown-with-msg?
             ParserException #"Could not resolve symbol c"
             (str (resolve "a + c")))))

      (testing "reference to field in struct"
        (is (equal?
             `((~'= 1 (. (:variables ~'s) (:fields ~'inner) (:fields ~'i))) :bool)
             (resolve "1 == s.inner.i"))))

      (testing "reference to unknown field in struct"
        (is (thrown-with-msg?
             ParserException #"Could not find field named a"
             (str (resolve "1 == s.inner.a"))))))

    (testing "if expression"
      (testing "simple if expression"
        (is (equal?
             `((if (~'= (:constants ~'someConst) (:variables ~'a)) 2 3) :int)
             (resolve "someConst == a ? 2 : 3"))))

      (testing "if expression with a non-bool condition"
        (is (thrown-with-msg?
             ParserException #"Condition should resolve to a boolean, got :int"
             (str (resolve "1 ? 2 : 3")))))

      (testing "if expression with branches that resolve to different types"
        (is (thrown-with-msg?
             ParserException #"Then and else branches of if expression should resolve to the same type, got :bool, :int"
             (str (resolve "1 == 2 ? true : 3"))))))

    (testing "array literals"
      (testing "simple expression"
        (is (equal?
             `([1 2 3] [:int])
             (resolve "[1, 2, 3]"))))

      (testing "simple expression with references"
        (is (equal?
             `([(:variables ~'a) (:parameters ~'b) (:constants ~'someConst)] [:int])
             (resolve "[a, b, someConst]"))))

      (testing "expression with different types"
        (is (thrown-with-msg?
             ParserException #"Type mismatch: Cannot have array literal with multiple types"
             (resolve "[1, true]")))))))


(deftest test-resolve-send
  (let [ns {:variables {'a (->Variable 'a :int)}
            :responses {'someResponse (make-response 'someResponse [(->Parameter 'b :int)])}}
        resolve (make-resolver :proc-send-stmt ns)]

    (testing "send with no parameters"
      (is (equal?
           `(:send (:responses ~'someResponse))
           (resolve "send someResponse()"))))

    (testing "send with parameters and arguments"
      (is (equal?
           `(:send (:responses ~'someResponse)
                   (~'= (:arguments ~'b) (~'+ (:variables ~'a) 1)))
           (resolve "send someResponse(.b == a + 1)"))))

    (testing "send with expr that does not match type parameter"
      (is (thrown-with-msg?
           ParserException #"= is not implemented for types \(:int :bool\)"
           (str (resolve "send someResponse(.b == true)")))))

    (testing "send with no such response"
      (is (thrown-with-msg?
           ParserException #"Could not find response named unknownResponse"
           (str (resolve "send unknownResponse(.a == a)")))))))


(deftest test-resolve-receive
  (let [ns {:stimuli {'someStimulus (->Stimulus 'someStimulus :int)}
            :variables {'someVariable (->Variable 'someVariable :int)
                        'someOtherVariable (->Variable 'someOtherVariable :bool)}}
        resolve (make-resolver :proc-receive-stmt ns)]

    (testing "receive with variable"

      (testing "and known stimulus"
        (is (equal?
             `(:receive (:variables ~'someVariable) (:stimuli ~'someStimulus))
             (resolve "someVariable = receive someStimulus"))))

      (testing "and unknown stimulus"
        (is (thrown-with-msg?
             ParserException #"Could not find stimulus unknownStimulus"
             (str (resolve "someVariable = receive unknownStimulus")))))

      (testing "and unknown variable"
        (is (thrown-with-msg?
             ParserException #"Could not find variable named unknownVariable"
             (str (resolve "unknownVariable = receive someStimulus")))))

      (testing "and with different type"
        (is (thrown-with-msg?
             ParserException #"Could not assign stimulus someStimulus of type :int to variable someOtherVariable"
             (resolve "someOtherVariable = receive someStimulus")))))

    (testing "receive without variable"
      (testing "and known stimulus"
        (is (equal?
             `(:receive ~nil (:stimuli ~'someStimulus))
             (resolve "receive someStimulus"))))

      (testing "and unknown stimulus"
        (is (thrown-with-msg?
             ParserException #"Could not find stimulus unknownStimulus"
             (str (resolve "receive unknownStimulus"))))))))


(deftest test-resolve-update
  (let [ns {:structs {'someStruct (make-struct 'someStruct :fields [(->Field 'a :int)])}
            :variables {'someVariable (->Variable 'someVariable :int)
                        'structVariable (->Variable 'structVariable '(:structs someStruct))}}
        resolve (make-resolver :proc-update-stmt ns)]
    (testing "with known variable"
      (is (equal?
           `(:update (:variables ~'someVariable) (~'+ 1 2))
           (resolve "someVariable = 1 + 2"))))

    (testing "with assignment to struct field"
      (is (equal?
           `(:update (. (:variables ~'structVariable) (:fields ~'a)) (~'+ 1 2))
           (resolve "structVariable.a = 1 + 2"))))

    (testing "with unknown variable"
      (is (thrown-with-msg?
           ParserException #"Could not find variable named unknownVariable"
           (str (resolve "unknownVariable = 1 + 2")))))

    (testing "with unresolvable symbol in expression"
      (is (thrown-with-msg?
           ParserException #"Could not resolve symbol unknownVariable"
           (str (resolve "someVariable = 1 + unknownVariable")))))

    (testing "with expression that resolves to wrong type for variable"
      (is (thrown-with-msg?
           ParserException #"Could not assign value of type :bool to variable of type :int"
           (str (resolve "someVariable = true")))))))


(deftest test-resolve-if
  (let [ns {:variables {'someVariable (->Variable 'someVariable :int)
                        'anotherVariable (->Variable 'anotherVariable :int)}}
        resolve (make-resolver :proc-if-stmt ns)]
    (testing "with expression that does not resolve to bool"
      (is (thrown-with-msg?
           ParserException #"Constraint expression should evaluate to bool, got :int"
           (str (resolve "if 1 + 1 { }")))))

    (testing "with else expression"
      (testing "with valid expression and block"
        (is (equal?
             `(:choice (:block (:constraint (~'< (:variables ~'someVariable) 10))
                               (:update (:variables ~'anotherVariable) 1))
                       (:block (:update (:variables ~'anotherVariable) 2)))
             (resolve "if someVariable < 10 { anotherVariable = 1 } else { anotherVariable = 2 }"))))

      (testing "with invalid expression and valid block"
        (is (thrown-with-msg?
             ParserException #"Could not resolve symbol unknownVariable"
             (str (resolve "if unknownVariable < 10 { anotherVariable = 1 } else { anotherVariable = 2 }")))))

      (testing "with valid expression and invalid then statement"
        (is (thrown-with-msg?
             ParserException #"Could not find variable named unknownVariable"
             (str (resolve "if someVariable < 10 { unknownVariable = 1 } else { someVariable = 2 }")))))

      (testing "with valid expression and invalid else statement"
        (is (thrown-with-msg?
             ParserException #"Could not find variable named unknownVariable"
             (str (resolve "if someVariable < 10 { anotherVariable = 1 } else { unknownVariable = 2 }"))))))

    (testing "without else expression"
      (testing "with valid expression and block"
        (is (equal?
             `(:choice (:block (:constraint (~'< (:variables ~'someVariable) 10))
                               (:update (:variables ~'anotherVariable) 1))
                       (:block))
             (resolve "if someVariable < 10 { anotherVariable = 1 }"))))

      (testing "with invalid expression and valid block"
        (is (thrown-with-msg?
             ParserException #"Could not resolve symbol unknownVariable"
             (str (resolve "if unknownVariable < 10 { anotherVariable = 1 }")))))

      (testing "with valid expression and invalid then statement"
        (is (thrown-with-msg?
             ParserException #"Could not find variable named unknownVariable"
             (str (resolve "if someVariable < 10 { unknownVariable = 1 }"))))))))


(deftest resolve-choice
  (let [ns {:variables {'v (->Variable 'v :int)}}
        resolve (make-resolver :proc-choice-stmt ns)]
    (testing "with valid blocks"
      (is (equal?
           `(:choice (:block (:update (:variables ~'v) 1))
                     (:block (:update (:variables ~'v) 2)))
           (resolve "choice { option { v = 1 } option { v = 2 } }"))))

    (testing "with an invalid blocks"
      (is (thrown-with-msg?
           ParserException #"Could not find variable named a"
           (str (resolve "choice { option { v = 1 } option { a = 2 } }")))))))
