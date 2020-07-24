(ns behave.dsl.rewriter
  (:require
   [behave.dsl.helpers :refer [type-from-value parser-error third]]
   [behave.ontology :refer :all]))

;; Takes the tree generated by the parser. In this tree, each inner
;;  node is a seq starting with the production rule and the leaves are
;;  string literals.

;; Caught errors:
;; - Elements with the same name and type within the same namespace are caught by the to-map within the make-xxx call.


(defn rewrite-dispatch-fn [expr]
  (case (first expr)
    ;; Rules of the form `rule = child-rule [ "," rule ]`
    (:argument-list-expr :parameter-name-list :proc-var-names
                          :struct-field-names :unnamed-argument-list
                          :attr-identifier :array-literal-contents)
    :iterating-production-rule
    ;; These productions are simply meant to group or rename others.
    (:argument-list :argument-expr :expr1 :expr2 :fn-name :fn-stmt :label-decl
                    :literal-expr :model-decl :parameter-name :proc-if-stmt
                    :proc-name :proc-receive-stmt :proc-state-name :proc-var-name :response-name
                    :stimulus-name :struct-field-name :type :unnamed-argument :unnamed-argument-expr
                    :var-expr :proc-body-stmt)
    :renaming-production-rule
    (:opt-parameter-list :opt-unnamed-argument-list :opt-argument-list-expr
                         :opt-array-literal-contents :opt-expr-start)
    :opt-list-production-rule
    ;; default
    (first expr)))

(defmulti rewrite #'rewrite-dispatch-fn)

(defmethod rewrite :model [[_ & stmts]]
  (let [items (map rewrite stmts)
        grouped (group-by type items)]
    (make-model :constants (get grouped behave.ontology.Constant [])
                :structs   (get grouped behave.ontology.Struct [])
                :functions (get grouped behave.ontology.Function [])
                :stimuli   (get grouped behave.ontology.Stimulus [])
                :responses (get grouped behave.ontology.Response [])
                :processes (get grouped behave.ontology.Process []))))

(defmethod rewrite :const-decl [[_ _ name _ lit]]
  (let [name (rewrite name)
        lit (rewrite lit)]
    (->Constant name lit (type-from-value lit))))

(defmethod rewrite :struct-decl [[_ _ name _ fields _]]
  (let [name (rewrite name)
        fields (rewrite fields)]
    (make-struct name :fields fields)))

(defmethod rewrite :struct-field-decls [[_ & decls]]
  (mapcat rewrite decls))

(defmethod rewrite :struct-field-decl [[_ names type]]
  (let [names (rewrite names)
        type (rewrite type)]
    (map #(->Field % type) names)))

(defmethod rewrite :stimulus-decl [[_ _ name type]]
  (let [name (rewrite name)
        type (rewrite type)]
    (->Stimulus name type)))

(defmethod rewrite :response-decl [[_ _ name _ params _]]
  (let [name   (rewrite name)
        params (rewrite params)]
    (make-response name params)))

(defmethod rewrite :fn-decl [[_ _ name _ params _ return _ expr _]]
  (let [name (rewrite name)
        return (rewrite return)
        params (rewrite params)
        expr (rewrite expr)]
    (make-function name return params expr)))

(defn- gather-states [node]
  (cond (not (seq? node))
        nil
        (= (first node) :state)
        (list (->State (second node)))
        true
        (mapcat gather-states node)))

(defmethod rewrite :proc-decl [[_ _ name _ params _ _ vars stmts _]]
  (let [name (rewrite name)
        params (rewrite params)
        vars (rewrite vars)
        stmts (rewrite stmts)
        states (gather-states stmts)
        body `(:block ~@stmts)]
    (make-process name
                  :parameters params
                  :variables vars
                  :body body
                  :init-state (first states)
                  :states states)))

(defmethod rewrite :proc-goto-stmt [[_ _ state]]
  `(:goto ~(rewrite state)))

(defmethod rewrite :proc-state-stmt [[_ stmt]]
  `(:state ~(rewrite stmt)))

(defmethod rewrite :proc-receiveandstore-stmt [[_ var _ _ stimulus]]
  `(:receive ~(rewrite var) ~(rewrite stimulus)))

(defmethod rewrite :proc-receiveandignore-stmt [[_ _ stimulus]]
  `(:receive nil ~(rewrite stimulus)))

(defmethod rewrite :proc-body-stmts [[_ & stmts]]
  (map rewrite stmts))

(defmethod rewrite :proc-option-stmts [[_ & options]]
  (map rewrite options))

(defmethod rewrite :proc-ifelse-stmt [[_ _ cond _ then _ _ _ else _]]
  `(:choice
    (:block (:constraint ~(rewrite cond)) ~@(rewrite then))
    ;; TODO: add constraint for negation of cond to then block
    (:block ~@(rewrite else))))

(defmethod rewrite :proc-onlyif-stmt [[_ _ cond _ then _]]
  `(:choice
    (:block (:constraint ~(rewrite cond)) ~@(rewrite then))
    ;; TODO: add constraint for negation of cond to alternative block
    (:block)))

(defmethod rewrite :proc-optionally-stmt [[_ _ _ stmts _]]
  `(:choice (:block ~@(rewrite stmts)) (:block)))

(defmethod rewrite :proc-send-stmt [[_ _ name _ expr _]]
  (let [expr (rewrite expr)]
    (if (empty? expr)
      `(:send ~(rewrite name))
      `(:send ~(rewrite name) ~expr))))

(defmethod rewrite :proc-choice-stmt [[_ _ _ options _]]
  `(:choice ~@(rewrite options)))

(defmethod rewrite :proc-option-stmt [[_ _ _ stmts _]]
  `(:block ~@(rewrite stmts)))

(defmethod rewrite :proc-update-stmt [[_ var _ expr]]
  `(:update ~(rewrite var) ~(rewrite expr)))

(defmethod rewrite :proc-spawn-stmt [[_ _ name _ args _]]
  `(:spawn ~(rewrite name) ~@(rewrite args)))

(defmethod rewrite :proc-var-stmts [[_ & stmts]]
  (mapcat rewrite stmts))

(defmethod rewrite :proc-var-stmt [[_ _ names type]]
  (let [names (rewrite names)
        type (rewrite type)]
    (map #(->Variable % type) names)))

(defmethod rewrite :parameter-list [[_ & items]]
  (if (= 1 (count items))
    (rewrite (first items))
    (concat (rewrite (first items))
            (rewrite (third items)))))

(defmethod rewrite :parameter [[_ names type]]
  (let [names (rewrite names)
        type (rewrite type)]
    (map #(->Parameter % type) names)))

;; Expressions
(defmethod rewrite :expr-start [[_ expr]]
  `(:expr ~(rewrite expr)))

(defmethod rewrite :if-expr [[_ cond _ then _ else _]]
  `(if ~(rewrite cond) ~(rewrite then) ~(rewrite else)))

(defmethod rewrite :fncall-expr [[_ fn-name _ arglist _]]
  (let [fn-name (rewrite fn-name)
        arglist (rewrite arglist)]
    `(~fn-name ~@arglist)))

(defmethod rewrite :op1 [[_ op]]
  (symbol op))

(defmethod rewrite :op2 [[_ op]]
  (symbol op))

(defmethod rewrite :opcall1-expr [[_ a op b]]
  (let [op (rewrite op)
        a (rewrite a)
        b (rewrite b)]
    (list op a b)))

(defmethod rewrite :opcall2-expr [[_ a op b]]
  (let [op (rewrite op)
        a (rewrite a)
        b (rewrite b)]
    (list op a b)))

(defmethod rewrite :reference [[_ ref & attrs]]
  (if (empty? attrs)
    (rewrite ref)
    (let [[_ attrs] attrs]
      `(. ~(rewrite ref)
          ~@(rewrite attrs)))))

(defmethod rewrite :identifier [[_ name]]
  (symbol name))

(defmethod rewrite :arg-identifier [[_ name]]
  (symbol name))

(defmethod rewrite :literal [[_ lit]]
  (rewrite lit))

(defmethod rewrite :boolean-literal [[_ lit]]
  (= "true" lit))

(defmethod rewrite :int-literal [[_ lit]]
  (Integer. lit))

(defmethod rewrite :string-literal [[_ lit]]
  (subs lit 1 (- (.length lit) 1)))

(defmethod rewrite :array-literal [[_ _ contents _]]
  (vec (rewrite contents)))

(defmethod rewrite :incl-range-literal [[_ start _ end]]
  (vec (range (Integer. start) (+ 1 (Integer. end)))))

(defmethod rewrite :void-type [[_]]
  nil)

(defmethod rewrite :basic-type [[_ name]]
  (keyword name))

(defmethod rewrite :in-expr [[_ ref _ range]]
  (let [ref (rewrite ref)
        range (rewrite range)]
    `(~'in ~ref ~range)))

(defmethod rewrite :iterating-production-rule [[_ & items]]
  (if (= 1 (count items))
    (list (rewrite (first items)))
    (cons (rewrite (first items))
          (rewrite (third items)))))

(defmethod rewrite :renaming-production-rule [[_ item]]
  (rewrite item))

(defmethod rewrite :opt-list-production-rule [[_ & opt]]
  (if (empty? opt)
    (list)
    (rewrite (first opt))))