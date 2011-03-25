(ns delimc.core
  (:import [clojure.lang Reflector]))

;; ================================================================================
;; Utilities
;; ================================================================================

(def ^:dynamic ctx nil)

(def not-seq? (comp not seq?))

(defn sym-to-key [sym]
  (keyword (str sym)))

;; ================================================================================
;; CPS Transformer
;; ================================================================================

(def special-form-transformers (ref {}))

(defmacro defcpstransformer [name lambda-list & body]
  `(dosync
    (commute special-form-transformers assoc ~(keyword (str name))
             (fn [~@lambda-list] ~@body))))

(defn cpstransformer [name]
  (name @special-form-transformers))

(defstruct call-cc-context :local-functions)

(defn make-call-cc-context []
  (struct call-cc-context nil))

;; ================================================================================
;; Helper Transformers
;; ================================================================================

(declare expr-sequence->cps
         apply->cps)

(defcpstransformer with-call-cc [cons k-expr]
  (expr-sequence->cps (rest cons) k-expr))

(defmacro without-call-cc [& body]
  `(do
     ~@body))

(defcpstransformer without-call-cc [cons k-expr]
  `(~k-expr (do ~@(rest cons))))

(defmacro defn-cc [name arglist & body]
  `(with-call-cc
     (~'defn ~name [~@arglist] ~@body)))

(declare lambda-expr->cps)

(defcpstransformer defn [[_ name args & body] k-expr]
  `(do
     (def ~name
          ~(lambda-expr->cps `(fn [~@args]
                                ~@body)
                             nil))
     (~k-expr ~name)))

(defcpstransformer apply [cons k-expr]
  (apply->cps (rest cons) k-expr nil))

;; ================================================================================
;; Walker
;; ================================================================================

(declare expr->cps
         atom->cps 
         cons->cps 
         application->cps 
         apply->cps
         funcall->cps
         expr-sequence->cps)

;; Gives access to call-cc by transforming body to continuation passing style."
(defmacro with-call-cc [& body]
  (binding [ctx (make-call-cc-context)]
    (expr-sequence->cps body identity)))

(defn expr->cps [expr k-expr]
  (if (not-seq? expr)
    (atom->cps expr k-expr)
    (cons->cps expr k-expr)))

(defn atom->cps [atom k-expr]
  `(~k-expr ~atom))

(defn funcall->cps [acons k-expr args]
  (application->cps 'funcall-cc acons k-expr args))

;; we need to mark functions for transformation
(def function identity)

(defn expanded? [original expansion]
  (not (= original expansion)))

(defn check-for-fn [form]
  (let [sym (first form)]
    (if (and (not (= sym 'fn))
             (not (= sym 'clojure.core/fn))
             (not (= sym 'clojure.core/fn*))
             (not (= sym 'fn*)))
      form
      `(~'function ~form))))

(defn cons->cps [acons k-expr]
  (let [acons       (check-for-fn acons)
        transformer ((sym-to-key (first acons)) @special-form-transformers)]
    (if transformer
      (transformer acons k-expr)
      (let [expansion (macroexpand-1 acons)
            expanded-p (expanded? acons expansion)]
        (if expanded-p
          (expr->cps expansion k-expr)
          (funcall->cps 
           (cons `(~'function ~(first expansion)) (rest expansion)) k-expr nil))))))

(defn application->cps [app-sym acons k-expr args]
  (if (seq acons)
    (expr->cps (first acons)
               (let [i         (gensym)
                     rest-args (gensym)]
                 `(fn [~i ~'& ~rest-args]
                    ~(application->cps app-sym (rest acons)
                                       k-expr
                                       (cons i args)))))
    (let [r-args (reverse args)]
      `(~app-sym ~(first r-args) ~k-expr ~@(rest r-args)))))

(defn apply->cps [acons k-expr args]
  (application->cps 'apply-cc acons k-expr args))

;; ================================================================================
;; Special form transformers
;; ================================================================================

(defn call-cc [cc]
  (throw (Exception. "Please ensure call-cc is called from within with-call-cc macro.")))

(defmacro let-cc [k & body]
  `(~'call-cc (fn [~k] ~@body)))

(defcpstransformer call-cc [cons k-expr]
  (if (not (= (count cons) 2))
    (throw (Exception. "Please ensure call-cc has one argument.")))
  `(~(first (rest cons)) ~k-expr))

;; quote
;; --------------------------------------------------------------------------------
(defcpstransformer quote 
  [acons k-expr]
  `(~k-expr ~acons))

;; do
;; --------------------------------------------------------------------------------
(defn expr-sequence->cps [expr-list k-expr]
  (expr->cps (first expr-list)
             (if (nil? (seq (rest expr-list)))
               k-expr
               (let [r         (gensym)
                     rest-args (gensym)]
                 `(fn [~r ~'& ~rest-args]
                    ~(expr-sequence->cps (rest expr-list) k-expr))))))

(defcpstransformer do [acons k-expr]
  (expr-sequence->cps (rest acons) k-expr))

;; let
;; --------------------------------------------------------------------------------
(defn let-varlist->cps [varlist let-body k-expr]
  (let [avar         (first varlist)
        avar-name    (if (seq? avar) (first avar) avar)
        avar-value   (if (seq? avar) (first (rest avar)))]
    (if (seq? avar)
      (expr->cps avar-value
                 (let [rest-args (gensym)]
                   `(fn [~avar-name ~'& ~rest-args]
                      ~(let-varlist->cps (rest varlist) let-body k-expr))))
      (expr-sequence->cps let-body k-expr))))

(defcpstransformer let [[_ varlist & forms] k-expr]
  (let-varlist->cps (partition 2 varlist) forms k-expr))

(dosync
 (commute special-form-transformers assoc :let* (:let @special-form-transformers)))

;; function
;; --------------------------------------------------------------------------------
(declare fdesignator-to-function-cc)

(defn funcall-cc [afn k & args]
  (apply (fdesignator-to-function-cc afn) k args))

(defn apply-cc [fdesignator k & args]
  (apply apply (fdesignator-to-function-cc fdesignator) k args))

;; Converts a lambda expression to CPS style.
(defn lambda-expr->cps [[_ arglist & body] k-expr]
  (let [k (gensym)]
    `(~'make-funcallable (fn [~k ~@arglist]
                           ~(expr-sequence->cps body k)))))

(defn make-funcallable [afn]
  (with-meta (fn [& args] (apply afn identity args)) {:funcallable true, :fn afn}))

(defn fdesignator-to-function-cc [afn]
  (if (:funcallable (meta afn))
    (:fn (meta afn))
    (fn [k & args]
      (k (apply afn args)))))

;; refactor
(defn is-fn? [fdesignator]
  (or (= fdesignator 'clojure.core/fn)
      (= fdesignator 'clojure.core/fn*)
      (= fdesignator 'fn)
      (= fdesignator 'fn*)))

(defcpstransformer function [[_ fdesignator :as acons] k-expr]
  (cond
   (not-seq? fdesignator) (if (some #{fdesignator} (:local-functions ctx))
                            `(~k-expr (make-funcallable ~acons))
                            `(~k-expr ~acons))
   (and (seq? (seq fdesignator))
        (is-fn? (first fdesignator))) `(~k-expr ~(lambda-expr->cps fdesignator k-expr))))

(defmacro fn-cc [args-list & body]
  `(with-call-cc
     (fn [~@args-list]
       ~@body)))

;; if
;; --------------------------------------------------------------------------------
(defcpstransformer if [[_ pred-expr pred-true-expr pred-false-expr] k-expr]
  (expr->cps pred-expr
             (let [pred      (gensym)
                   rest-args (gensym)]
               `(fn [~pred ~'& ~rest-args]
                  (if ~pred
                    ~(expr->cps pred-true-expr k-expr)
                    ~(expr->cps pred-false-expr k-expr))))))
(dosync
 (commute special-form-transformers assoc :if* (:if @special-form-transformers)))

;; letfn
;; --------------------------------------------------------------------------------
(defmacro transform-forms-in-env [forms k-expr transf-env]
  (binding [ctx transf-env]
    (expr-sequence->cps forms k-expr)))

(defn transform-local-function [[fn-name fn-args & fn-forms :as afn]]
  (let [k (gensym)]
    (if (and fn-name (symbol? fn-name))
      nil
      (throw (Exception. "Function name must be non-nil symbol")))
    (if (>= (count afn) 2)
      nil
      (throw (Exception. "Function arguments not specified")))
    `(~fn-name [~k ~@fn-args]
               (transform-forms-in-env ~fn-forms ~k ~ctx))))

(defn declare-function-names-local [fnames]
  (loop [result (:local-functions ctx) names fnames]
    (if (= (seq names) nil)
      result
      (recur (conj result (first names)) (rest names)))))

(defmacro with-local-function-names [names & body]
  (let [fn-list (gensym)]
    `(let [~fn-list ~names]
       (do
         (binding [ctx (assoc ctx :local-functions 
                                (declare-function-names-local ~fn-list))]
           ~@body)))))

(defcpstransformer letfn [[_ fn-list & forms :as acons] k-expr]
  (if (>= (count acons) 2)
    nil
    (throw (Exception. "Too few parameters to letfn")))
  (with-local-function-names 
    (map first fn-list)
    `(letfn [~@(map (fn [afn]
                      (transform-local-function afn))
                 fn-list)]
       (transform-forms-in-env ~forms ~k-expr ~ctx))))

;; dot
;; --------------------------------------------------------------------------------
(defn dot [obj member-expr]
  (let [member       (name (first member-expr))
        arg-or-args  (rest member-expr)
        args         (to-array (if (not-seq? arg-or-args)
                                 [arg-or-args]
                                 arg-or-args))]
    (if (= (type obj) java.lang.Class)
      (Reflector/invokeStaticMethod obj member args)
      (Reflector/invokeInstanceMethod obj member args))))

;; TODO: look at this.
(defcpstransformer .
  [acons k-expr]
  (let [expr        (rest acons)
        obj         (first expr)
        member-expr (if (= (count (rest expr)) 1)
                      (rest expr)
                      (list (rest expr)))
        member      (first (first member-expr))
        args        (rest (first member-expr))]
    (expr->cps `(dot ~obj (list (quote ~member) ~@args)) k-expr)))

;; new
;; --------------------------------------------------------------------------------
(defn constructor [class & args]
  (Reflector/invokeConstructor class (to-array args)))

(defcpstransformer new
  [[_ obj & args] k-expr]
  (expr->cps `(constructor ~obj ~@args) k-expr))
