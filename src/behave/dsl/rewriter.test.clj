(ns behave.dsl.rewriter.test
  (:require
   [behave.dsl.parser :as parser]
   [behave.dsl.rewriter :refer :all]
   [behave.ontology :refer :all]
   [clojure.test :as t :refer [deftest is testing run-tests]]))
(load "/behave/test/helpers")

(deftest rewrite-model
  (testing "rewrite model"
    (is (equal?
         (make-model :constants [(->Constant 'someConstant 42 :int)]
                     :structs   [(make-struct 'someStruct :fields [])]
                     :domains   [(->Domain 'someDomain :int '((:expr 1) (:expr 2) (:expr 3)))]
                     :functions [(make-function 'someFunction :int [] '(:expr 0))]
                     :stimuli   [(->Stimulus 'someStimulus :int)]
                     :responses [(->Response 'someResponse {})]
                     :processes [(->Process 'someProcess {} {} '(:block) nil {} [])])
         (rewrite (parser/parse
                   "const someConstant = 42
                    type someStruct struct {}
                    type someDomain domain int [1, 2, 3]
                    func someFunction() int { 0 }
                    stimulus someStimulus int
                    response someResponse()
                    proc someProcess() {}"
                   :model))))))


(deftest rewrite-const-decl
  (testing "rewrite const-decl"
    (is (equal?
         (->Constant 'SomeConstant 42 :int)
         (rewrite (parser/parse "const SomeConstant = 42" :const-decl))))))


(deftest rewrite-struct-decl
  (testing "rewrite struct-decl with multiple params on one row"
    (is (equal?
         (->Struct 'httpResponse
                   {'foo (->Field 'foo :int)
                    'bar (->Field 'bar :int)
                    'baz (->Field 'baz :string)})
         (rewrite (parser/parse "type httpResponse struct { foo, bar int baz string }" :struct-decl))))))


(deftest rewrite-stimulus-decl
  (testing "rewrite stimulis-decl"
    (is (equal?
         (->Stimulus 'foo :int)
         (rewrite (parser/parse "stimulus foo int" :stimulus-decl))))))


(deftest rewrite-response-decl
  (testing "rewrite response-decl"
    (is (equal?
         (->Response 'httpRequest
                     {'url (->Parameter 'url :string)
                      'method (->Parameter 'method :string)
                      'body (->Parameter 'body :string)})
         (rewrite (parser/parse "response httpRequest(url, method, body string)" :response-decl))))))


(deftest rewrite-fn-decl
  (testing "rewrite fn-decl"
    (is (equal?
         (->Function 'add :int
                     {'x (->Parameter 'x :int)
                      'y (->Parameter 'y :int)}
                     `(:expr (~'+ ~'x ~'y)))
         (rewrite (parser/parse "func add(x, y int) int { x + y }" :fn-decl))))))


(deftest rewrite-proc-decl
  (testing "rewrite proc-decl"
    (is (equal?
         (make-process 'handleWebhook
                       :parameters [(->Parameter 'w 'webhook)]
                       :variables [(->Variable 'attempt :int) (->Variable 'resp 'httpResponse)]
                       :body '(:block (:state waiting)
                                      (:choice
                                       (:block (:send someResponse))
                                      (:block (:send someOtherResponse)))
                                     (:state done)
                                     (:goto waiting))
                      :init-state (->State 'waiting)
                      :states [(->State 'waiting) (->State 'done)]
                      :transitions [])
         (rewrite
          (parser/parse
           "proc handleWebhook(w webhook) {
             var attempt int
             var resp httpResponse
            waiting: 
             choice {
              option { send someResponse() }
              option { send someOtherResponse() }
             }
            done:
             goto waiting
            }"
           :proc-decl))))))


(deftest rewrite-proc-goto-stmt
  (testing "rewrite proc-goto-stmt"
    (is (equal?
         '(:goto foo)
         (rewrite (parser/parse "goto foo" :proc-goto-stmt))))))


(deftest rewrite-proc-state-stmt
  (testing "rewrite proc-state-stmt"
    (is (equal?
         '(:state foo)
         (rewrite (parser/parse "foo:" :proc-state-stmt))))))


(deftest rewrite-proc-receiveandstore-stmt
  (testing "rewrite proc-receiveandstore-stmt"
    (is (equal?
         '(:receive var someStimulus)
         (rewrite (parser/parse "var = receive someStimulus" :proc-receiveandstore-stmt))))))


(deftest rewrite-proc-receiveandignore-stmt
  (testing "rewrite proc-receiveandignore-stmt"
    (is (equal?
         '(:receive nil someStimulus)
         (rewrite (parser/parse "receive someStimulus" :proc-receiveandignore-stmt))))))


(deftest rewrite-proc-ifelse-stmt
  (testing "rewrite proc-ifelse-stmt with single item per block"
    (is (equal?
         '(:choice (:block (:constraint (:expr (someFunction))) (:receive var someLabel)) (:block (:receive nil someOtherLabel)))
         (rewrite (parser/parse "if someFunction() { var = receive someLabel } else { receive someOtherLabel }" :proc-ifelse-stmt)))))

  (testing "rewrite proc-ifelse-stmt with multiple items per block"
    (is (equal?
         '(:choice (:block (:constraint (:expr (someFunction))) (:receive var someLabel) (:receive var someOtherLabel)) (:block (:receive nil someLabel) (:receive nil someOtherLabel)))
         (rewrite (parser/parse "if someFunction() { var = receive someLabel var = receive someOtherLabel } else { receive someLabel receive someOtherLabel }" :proc-ifelse-stmt))))))


(deftest rewrite-proc-onlyif-stmt
  (testing "rewrite proc-onlyif-stmt with single item in block"
    (is (equal?
         '(:choice (:block (:constraint (:expr (someFunction))) (:receive var someLabel)) (:block))
         (rewrite (parser/parse "if someFunction() { var = receive someLabel }" :proc-onlyif-stmt)))))
  
  (testing "rewrite proc-onlyif-stmt with multiple items in block"
    (is (equal?
         '(:choice (:block (:constraint (:expr (someFunction))) (:receive var someLabel) (:receive nil someOtherLabel)) (:block))
         (rewrite (parser/parse "if someFunction() { var = receive someLabel receive someOtherLabel }" :proc-onlyif-stmt))))))


(deftest rewrite-proc-optionally-stmt
  (testing "rewrite proc-optionally-stmt with single item in block"
    (is (equal?
         '(:choice (:block (:receive var someLabel)) (:block))
         (rewrite (parser/parse "optionally { var = receive someLabel }" :proc-optionally-stmt)))))
  (testing "rewrite proc-onlyif-stmt with multiple items in block"
    (is (equal?
         '(:choice (:block (:receive var someLabel) (:receive nil someOtherLabel)) (:block))
         (rewrite (parser/parse "optionally { var = receive someLabel receive someOtherLabel }" :proc-optionally-stmt))))))


(deftest rewrite-proc-choice-stmt
  (testing "rewrite proc-choice-stmt"
    (is (equal?
         '(:choice (:block (:update x (:expr 1))) (:block (:update y (:expr 2))))
         (rewrite (parser/parse "choice { option { x = 1 } option { y = 2 } }" :proc-choice-stmt))))))


(deftest rewrite-proc-update-stmt
  (testing "rewrite proc-update-stmt to simple variable"
    (is (equal?
         '(:update var (:expr (someFunction a b)))
         (rewrite (parser/parse "var = someFunction(a, b)" :proc-update-stmt)))))
  (testing "rewrite proc-update-stmt to struct field"
    (is (equal?
         '(:update (. s a b) (:expr (someFunction a b)))
         (rewrite (parser/parse "s.a.b = someFunction(a, b)" :proc-update-stmt))))))

(deftest rewrite-proc-send-stmt
  (testing "rewrite proc-send-stmt without parameters"
    (is (equal?
         '(:send httpRequest)
         (rewrite (parser/parse "send httpRequest()" :proc-send-stmt)))))
  
  (testing "rewrite proc-send-stmt with single named parameter"
    (is (equal?
         '(:send httpRequest (:expr (== .url u)))
         (rewrite (parser/parse "send httpRequest(.url == u)" :proc-send-stmt)))))
  
  (testing "rewrite proc-send-stmt with multiple named parameters"
    (is (equal?
         '(:send httpRequest (:expr (&& (== .url u) (== .webhook w))))
         (rewrite (parser/parse "send httpRequest(.url == u && .webhook == w)" :proc-send-stmt))))))


(deftest rewrite-proc-spawn-stmt
  (testing "rewrite proc-spawn-stmt with no parameters"
    (is (equal?
         '(:spawn api)
         (rewrite (parser/parse "spawn api()" :proc-spawn-stmt)))))
  
  (testing "rewrite proc-spawn-stmt with single unnamed parameter"
    (is (equal?
         '(:spawn api (:expr a))
         (rewrite (parser/parse "spawn api(a)" :proc-spawn-stmt)))))
  
  (testing "rewrite proc-spawn-stmt with multiple unnamed parameters"
    (is (equal?
         '(:spawn api (:expr a) (:expr b))
         (rewrite (parser/parse "spawn api(a, b)" :proc-spawn-stmt))))))


(deftest rewrite-proc-var-stmt
  (testing "rewrite proc-var-stmt with single name"
    (is (equal?
         [(->Variable 'foo :int)]
         (rewrite (parser/parse "var foo int" :proc-var-stmt)))))
 
  (testing "rewrite proc-var-stmt with multiple names"
    (is (equal?
         [(->Variable 'foo :int) (->Variable 'bar :int) (->Variable 'baz :int)]
         (rewrite (parser/parse "var foo, bar, baz int" :proc-var-stmt))))))


(deftest rewrite-fncall-expr
  (testing "rewrite fncall-expr without parameters"
    (is (equal?
         '(someFunction)
         (rewrite (parser/parse "someFunction()" :fncall-expr)))))
  
  (testing "rewrite fncall-expr with parameters"
    (is (equal?
         '(someFunction a b c)
         (rewrite (parser/parse "someFunction(a, b, c)" :fncall-expr))))))


(deftest rewrite-opcall-expr
  (testing "rewrite opcall-expr"
    (is (equal?
         '(< a b)
         (rewrite (parser/parse "a < b" :opcall2-expr))))))


(deftest rewrite-expr-start
  (testing "rewrite expr-start"
    (is (equal?
         '(:expr (< a b))
         (rewrite (parser/parse "a < b" :expr-start)))))
  (testing "rewrite if expression"
    (is (equal?
         '(:expr (if (== 1 2) true false))
         (rewrite (parser/parse "1 == 2 ? true : false" :expr-start))))))


(deftest rewrite-reference
  (testing "rewrite single reference"
    (is (equal?
         'a
         (rewrite (parser/parse "a" :reference)))))
  (testing "rewrite dotted reference"
    (is (equal?
         '(. a b)
         (rewrite (parser/parse "a.b" :reference)))))
  (testing "rewrite 3-times dotted reference"
    (is (equal?
         '(. a b c)
         (rewrite (parser/parse "a.b.c" :reference))))))


(deftest rewrite-iterating-production-rule
  (testing "comma separated literals"
    (is (equal?
         '(foo bar baz)
         (rewrite (parser/parse "foo, bar, baz" :struct-field-names))))))


(deftest rewrite-domain-decl
  (testing "domain declaration with only literals"
    (is (equal?
         (->Domain 'Foo :bool '((:expr true) (:expr false)))
         (rewrite (parser/parse "type Foo domain bool [true, false]" :domain-decl)))))
  (testing "domain declaration with only identifiers"
    (is (equal?
         (->Domain 'Foo :int '((:expr Bar) (:expr Baz) (:expr Quux)))
         (rewrite (parser/parse "type Foo domain int [Bar, Baz, Quux]" :domain-decl))))))
