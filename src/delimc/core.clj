(ns delimc.core)

;; ================================================================================
;; Utilities
;; ================================================================================

(def ^:dynamic *ctx* nil)

;; ================================================================================
;; CPS Transformer
;; ================================================================================

(defmulti transform (fn [[op & forms] k-expr] (keyword op)))

(defrecord Context [local-functions])

;; ================================================================================
;; Helper Transformers
;; ================================================================================

(declare expr-sequence->cps
         apply->cps)

(defmethod transform :reset [cons k-expr]
  (expr-sequence->cps (rest cons) k-expr))

(defmacro unreset [& body]
  `(do
     ~@body))

(defmethod transform :unreset [cons k-expr]
  `(~k-expr (do ~@(rest cons))))

(declare lambda-expr->cps)

(defmethod transform :defn [[_ name args & body] k-expr]
  `(do
     (def ~name
          ~(lambda-expr->cps `(fn [~@args]
                                ~@body)
                             nil))
     (~k-expr ~name)))

(defmethod transform :apply [cons k-expr]
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
(defmacro reset [& body]
  (binding [*ctx* (Context. nil)]
    (expr-sequence->cps body identity)))

(defn expr->cps [expr k-expr]
  (if (seq? expr)
    (cons->cps expr k-expr)
    (atom->cps expr k-expr)))

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

(defmethod transform :default [acons k-expr]
  (let [expansion (macroexpand-1 acons)
        expanded-p (expanded? acons expansion)]
    (if expanded-p
      (expr->cps expansion k-expr)
      (funcall->cps
       (cons `(~'function ~(first expansion)) (rest expansion)) k-expr nil))))

(defn cons->cps [acons k-expr]
  (transform (check-for-fn acons) k-expr))

(defn application->cps [app-sym acons k-expr args]
  (if (seq acons)
    (expr->cps (first acons)
               (let [i (gensym)]
                 `(fn [~i ~'& rest-args#]
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

(defn shift* [cc]
  (throw (Exception. "Please ensure shift is called from within the reset macro.")))

(defmacro shift [k & body]
  `(~'shift* (fn [~k] ~@body)))

(defmethod transform :shift* [cons k-expr]
  (if (not (= (count cons) 2))
    (throw (Exception. "Please ensure shift has one argument.")))
  `(~(first (rest cons)) ~k-expr))

;; quote
;; --------------------------------------------------------------------------------
(defmethod transform :quote
  [acons k-expr]
  `(~k-expr ~acons))

;; do
;; --------------------------------------------------------------------------------
(defn expr-sequence->cps [expr-list k-expr]
  (expr->cps (first expr-list)
             (if (nil? (seq (rest expr-list)))
               k-expr
               `(fn [r# ~'& rest-args#]
                  ~(expr-sequence->cps (rest expr-list) k-expr)))))

(defmethod transform :do [acons k-expr]
  (expr-sequence->cps (rest acons) k-expr))

;; let
;; --------------------------------------------------------------------------------
(defn let-varlist->cps [varlist let-body k-expr]
  (let [avar         (first varlist)
        avar-name    (if (seq? avar) (first avar) avar)
        avar-value   (if (seq? avar) (first (rest avar)))]
    (if (seq? avar)
      (expr->cps avar-value
                 `(fn [~avar-name ~'& rest-args#]
                    ~(let-varlist->cps (rest varlist) let-body k-expr)))
      (expr-sequence->cps let-body k-expr))))

(defmethod transform :let [[_ varlist & forms] k-expr]
  (let-varlist->cps (partition 2 varlist) forms k-expr))

(defmethod transform :let* [[_ & forms] k-expr]
  (transform (cons :let forms) k-expr))

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

(defmethod transform :function [[_ fdesignator :as acons] k-expr]
  (cond
   (not (seq? fdesignator)) (if (some #{fdesignator} (:local-functions *ctx*))
                              `(~k-expr (make-funcallable ~acons))
                              `(~k-expr ~acons))
   (and (seq? (seq fdesignator))
        (is-fn? (first fdesignator))) `(~k-expr ~(lambda-expr->cps fdesignator k-expr))))

(defmacro fn-cc [args-list & body]
  `(reset
     (fn [~@args-list]
       ~@body)))

;; if
;; --------------------------------------------------------------------------------
(defmethod transform :if [[_ pred-expr pred-true-expr pred-false-expr] k-expr]
  (expr->cps pred-expr
             `(fn [pred# ~'& rest-args#]
                (if pred#
                  ~(expr->cps pred-true-expr k-expr)
                  ~(expr->cps pred-false-expr k-expr)))))

(defmethod transform :if* [[_ & forms] k-expr]
  (transform (cons :if forms) k-expr))

;; letfn
;; --------------------------------------------------------------------------------
(defmacro transform-forms-in-env [forms k-expr transf-env]
  (binding [*ctx* transf-env]
    (expr-sequence->cps forms k-expr)))

(defn transform-local-function [[fn-name fn-args & fn-forms :as afn]]
  (when-not (and fn-name (symbol? fn-name))
    (throw (Exception. "Function name must be non-nil symbol")))
  (when (< (count afn) 2)
    (throw (Exception. "Function arguments not specified")))
  `(~fn-name [k# ~@fn-args]
             (transform-forms-in-env ~fn-forms k# ~*ctx*)))

(defn declare-function-names-local [fnames]
  (loop [result (:local-functions *ctx*) names fnames]
    (if (= (seq names) nil)
      result
      (recur (conj result (first names)) (rest names)))))

(defmacro with-local-function-names [names & body]
  `(let [fn-list# ~names]
     (do
       (binding [*ctx* (assoc *ctx* :local-functions
                            (declare-function-names-local fn-list#))]
         ~@body))))

(defmethod transform :letfn [[_ fn-list & forms :as acons] k-expr]
  (when (< (count acons) 2)
    (throw (Exception. "Too few parameters to letfn")))
  (with-local-function-names
    (map first fn-list)
    `(letfn [~@(map (fn [afn]
                      (transform-local-function afn))
                 fn-list)]
       (transform-forms-in-env ~forms ~k-expr ~*ctx*))))
