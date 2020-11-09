(ns behave.dsl.resolver
  (:require
   [behave.ontology :refer :all]
   [behave.dsl.helpers :refer [parser-error]]
   [clojure.algo.generic.functor :refer [fmap]]
   [clojure.string :refer [starts-with?]])
  (:import
   [behave.ontology Model Domain Struct Function Stimulus Response Field Parameter Variable Constant]))
(ns-unmap *ns* 'Process)
(import [behave.ontology Process])

;; Takes the tree generated by the rewriter (rewritten tree) and
;; resolves all symbols that point to variables, functions, parameters
;; etc.  The resulting tree should have the same shape as the
;; rewritten tree except that the relevant symbols are replaced by the relevant
;; records.

;; Caught erors:
;; - Unknown references to parameters, variables, etc.
;; - Unknown types. At the moment, only int, bool, string and defined structs and domains are valid types.
;; - Type mismatch. Taking inspiration from Go, you cannot cast int to float or vv without explicit casting
;; - Array types with more than one type. The values in an array or domain can only have a single value.

(defn resolve-symbol-expr [sym ns]
  "Looks up symbol in namespace fields :arguments, :parameters, :variables and :constants. In that order."
  (or
   (when (starts-with? sym ".")
     (let [sym (symbol (subs (name sym) 1))]
       (when-let [arg (get (:arguments ns) sym)]
         `((:arguments ~sym) ~(:type arg)))))
   (when-let [param (get (:parameters ns) sym)]
     `((:parameters ~sym) ~(:type param)))
   (when-let [var (get (:variables ns) sym)]
     `((:variables ~sym) ~(:type var)))
   (when-let [const (get (:constants ns) sym)]
     `((:constants ~sym) ~(:type const)))
   (parser-error "Could not resolve symbol ~a" sym)))

(defn resolve-type [sym ns]
  (or
   (when (get (:structs ns) sym)
     `(:structs ~sym))
   (when (get (:domains ns) sym)
     `(:domains ~sym))
   (parser-error "Could not resolve type ~a" sym)))

(defn resolve-references-dispatch-fn [item & namespaces]
  (if (seq? item)
    (first item)
    (type item)))

(defmulti resolve-references #'resolve-references-dispatch-fn)

(defmethod resolve-references Model [model]
  (reduce (fn [m f]
            (assoc m f (fmap #(resolve-references % m) (get m f))))
          model
          [:structs :domains :functions :stimuli :responses :processes]))

(defmethod resolve-references Domain [domain ns]
  (let [{:keys [name type values]} domain]
    (let [values (map #(resolve-references % ns) values)
          value-types (vec (distinct (map second values)))
          different (filter #(not (= type %)) value-types)]
      (if (seq different)
        (parser-error "Type mismatch: Domain with value ~a contains values with types: ~{~a~^, ~}" type different)
        (assoc domain :values (map first values))))))

(defmethod resolve-references Struct [struct ns]
  (let [{:keys [fields]} struct]
    (assoc struct
           :fields (fmap #(resolve-references % ns) fields))))

(defmethod resolve-references Field [field ns]
  (let [{:keys [type]} field]
    (if (keyword? type)
      field
      (assoc field :type (resolve-type type ns)))))

(defmethod resolve-references Function [func ns]
  (let [{:keys [return-type parameters expr]} func
        return-type (if (keyword? return-type)
                      return-type
                      (resolve-type return-type (dissoc ns :domains)))
        parameters (fmap #(resolve-references % ns) parameters)
        expr-ns (assoc ns :parameters parameters)
        [expr expr-type] (resolve-references expr expr-ns)]
    (if (not (= return-type expr-type))
      (parser-error "Function is declared to return type ~a but expression resolves to type ~a" return-type expr-type))
    (assoc func
             :return-type return-type
             :parameters parameters
             :expr expr)))

(defmethod resolve-references Parameter [parameter ns]
  (let [{:keys [type]} parameter]
    (if (keyword? type)
      parameter
      (assoc parameter :type (resolve-type type (dissoc ns :domains))))))

(defmethod resolve-references Stimulus [stimulus ns]
  (let [{:keys [type]} stimulus]
    (cond
      (nil? (:type stimulus)) stimulus
      (keyword? type) stimulus
      (type (:structs ns)) (assoc stimulus :type `(:structs ~type))
      :else (parser-error "Could not resolve type ~a" type))))

(defmethod resolve-references Response [response ns]
  (let [{:keys [parameters]} response]
    (assoc response
           :parameters (fmap #(resolve-references % ns) parameters))))

(defmethod resolve-references Process [process ns]
  (let [{:keys [parameters variables body states]} process
        parameters (fmap #(resolve-references % ns) parameters)
        variables (fmap #(resolve-references % ns) variables)
        process-ns (assoc ns                          :parameters parameters
                          :variables variables
                          :states states)
        body (resolve-references body process-ns)]
    (assoc process
           :parameters parameters
           :variables variables
           :body body)))

(defmethod resolve-references Variable [variable ns]
  (let [{:keys [type]} variable]
    (cond
      (keyword? type) variable
      (type (:structs ns)) (assoc variable :type `(:structs ~type))
      :else (parser-error "Could not resolve type ~a" type))))

(defmethod resolve-references :block [[_ & items] ns]
  `(:block ~@(map #(resolve-references % ns) items)))

(defmethod resolve-references :state [[_ name] ns]
  `(:state ~(or (get (:states ns) name)
                (parser-error "Unknown state named ~a" name))))

(defmethod resolve-references :goto [[_ name] ns]
  `(:goto ~(or (get (:states ns) name)
               (parser-error "State ~a is not defined" name))))

(defmethod resolve-references :spawn [[_ name & args] ns]
  (let [spawned (or (get (:processes ns) name)
                    (parser-error "Unknown process named ~a" name))
        spawn-params (vals (fmap #(resolve-references % ns) (:parameters spawned)))
        spawn-param-types (map :type spawn-params)
        args-with-types (map #(resolve-references % ns) args)
        args (map first args-with-types)
        arg-types (map second args-with-types)]
    (if (= spawn-param-types arg-types)
      `(:spawn ~name ~@args)
      (parser-error "Cannot use arg types ~a for param types ~a" arg-types spawn-param-types))))

(defn resolve-dot-expr [[_ struct-name & field-names] ns]
  (letfn [(resolve-ref [ref]
            (cond
              (keyword? ref) ref
              :else (let [[type name] ref]
                      (get (get ns type) name))))]
    (let [[[keyword _] struct-type] (resolve-symbol-expr struct-name ns)
          final-type (reduce (fn [s f]
                               (resolve-ref (:type (or (get (:fields s) f)
                                                       (parser-error "Could not find field named ~a" f)))))
                             (resolve-ref struct-type)
                             field-names)]
      `((. (~keyword ~struct-name) ~@(map #(list :fields %) field-names)) ~final-type))))

(defmethod resolve-references :expr [[_ expr] ns]
  (letfn [(resolve-expr [expr]
            (cond
              (seq? expr) (let [[fn-name & args] expr]
                            (if (= '. fn-name)
                               (resolve-dot-expr expr ns)
                               (let [args-with-types (map resolve-expr args)
                                     args (map first args-with-types)
                                     arg-types (map second args-with-types)]
                                 (case fn-name
                                   + (cond
                                       (= arg-types [:int :int])     `((~'+ ~@args) :int)
                                       (= arg-types [:float :float]) `((~'+ ~@args) :float)
                                       :else (parser-error "+ is not implemented for types ~a" arg-types))
                                   < (cond
                                       (= arg-types [:int :int])     `((~'< ~@args) :bool)
                                       (= arg-types [:float :float]) `((~'< ~@args) :bool)
                                       :else (parser-error "< is not implemented for types ~a" arg-types))
                                   == (if (and (= (first arg-types)
                                                  (second arg-types)))
                                        `((~'= ~@args) :bool)
                                        (parser-error "= is not implemented for types ~a" arg-types))
                                   && (if (= arg-types [:bool :bool])
                                        `((~'and ~@args) :bool)
                                        (parser-error "&& needs both args to resolve to a bool, got ~a" arg-types))
                                   if (let [[cond-expr then-expr else-expr] args
                                            [cond-type then-type else-type] arg-types]
                                        (cond
                                          (not (= cond-type :bool)) (parser-error "Condition should resolve to a boolean, got ~a" cond-type)
                                          (not (= then-type else-type)) (parser-error "Then and else branches of if expression should resolve to the same type, got ~a, ~a" then-type else-type)
                                          :else `((if ~cond-expr ~then-expr ~else-expr) ~then-type)))
                                   in (let [[member-expr col-expr] args
                                            [member-type col-type] arg-types]
                                        (if (= [member-type] col-type)
                                          `((~'in ~member-expr ~col-expr) :bool)
                                          (parser-error "Type mismatch: ~a cannot be in ~a" member-type col-type)))
                                   (let [fn (or (fn-name (:functions ns))
                                                (parser-error "Could not find function named ~a" fn-name))
                                         fn-params (vals (:parameters fn))
                                         fn-param-types (map :type fn-params)]
                                     (if (= fn-param-types arg-types)
                                       `(((:fn ~fn-name) ~@args) ~(:return-type fn))
                                       (parser-error "Cannot use arg types ~a for param types ~a" arg-types fn-param-types)))))))
              (vector? expr) (let [content-exprs (map resolve-expr expr)
                                   contents (map first content-exprs)
                                   content-types (vec (distinct (map second content-exprs)))]
                               (if (<= (count content-types) 1)
                                 `([~@contents] ~content-types)
                                 (parser-error "Type mismatch: Cannot have array literal with multiple types")))
              (symbol? expr) (resolve-symbol-expr expr ns)
              (int? expr) `(~expr :int)
              (boolean? expr) `(~expr :bool)
              (string? expr) `(~expr :string)
              :else (parser-error "Unknown expression ~a" expr)))]
    (resolve-expr expr)))

(defmethod resolve-references :send [[_ response-name expr] ns]
  (let [response (or (get (:responses ns) response-name)
                     (parser-error "Could not find response named ~a" response-name))
        ns (assoc ns :arguments (:parameters response))
        [expr expr-type] (when expr
                           (resolve-references expr ns))]
    (cond
      (not expr) `(:send (:responses ~response-name))
      (not (= :bool expr-type)) (parser-error "send expression should resolve to a boolean, got ~a" expr-type)
      :else `(:send (:responses ~response-name) ~expr))))

(defmethod resolve-references :receive [[_ var-name stimulus-name] ns]
  (let [var (when var-name
              (or (get (:variables ns) var-name)
                  (parser-error "Could not find variable named ~a" var-name)))
        stimulus (or (get (:stimuli ns) stimulus-name)
                     (parser-error "Could not find stimulus ~a" stimulus-name))]
    (cond
      (not var)                        `(:receive nil (:stimuli ~stimulus-name))
      (= (:type var) (:type stimulus)) `(:receive (:variables ~var-name) (:stimuli ~stimulus-name))
      :else (parser-error "Could not assign stimulus ~a of type ~a to variable ~a" (:name stimulus) (:type stimulus) var-name))))

(defmethod resolve-references :update [[_ var-expr expr] ns]
  (let [[var-expr var-type] (or (when (seq? var-expr)
                                  (resolve-dot-expr var-expr ns))
                                (when-let [var (get (:variables ns) var-expr)]
                                  `((:variables ~var-expr) ~(:type var)))
                         (parser-error "Could not find variable named ~a" var-expr))
        [expr expr-type] (resolve-references expr ns)]
    (if (= var-type expr-type)
      `(:update ~var-expr ~expr)
      (parser-error "Could not assign value of type ~a to variable of type ~a" expr-type var-type))))

(defmethod resolve-references :constraint [[_ expr] ns]
  (let [[expr expr-type] (resolve-references expr ns)]
    (if (= :bool expr-type)
      `(:constraint ~expr)
      (parser-error "Constraint expression should evaluate to bool, got ~a" expr-type))))

(defmethod resolve-references :choice [[_ & blocks] ns]
  (let [blocks (map #(resolve-references % ns) blocks)]
    `(:choice ~@blocks)))
