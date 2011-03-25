(ns delimc.test.core
  (:use [delimc.core] :reload)
  (:use [clojure.test]))

;; with-call-cc and global environment
(defmacro with-call-cc-env-test-macro [a]
  `(+ ~a ~a))

(defn-cc with-call-cc-env-test-fn []
  (with-call-cc-env-test-macro 5))

(deftest with-call-cc-env
  (is (= (with-call-cc
           (with-call-cc-env-test-fn))
         10)))

;; not-seq
(deftest not-seq-1
  (is (= (with-call-cc 1) 1)))

(deftest not-seq-2
  (is (= (let [cc (atom nil)]
           [(with-call-cc 
              (let-cc k
                      (reset! cc k)
                      (@cc 1)))
            (@cc 2)])
         [1 2])))

;; funcall
(deftest funcall-1 
  (is (= (with-call-cc (+ 1 2)) 
         3)))

(deftest funcall-2
  (is (= (with-call-cc (- (+ 1 2) 1))
         2)))

(deftest funcall-3
  (is (= (let [cc (atom nil)]
           [(with-call-cc
              (+ (let-cc k
                         (reset! cc k)
                         (k 1))
                 2))
            (@cc 2)
            (@cc 3)])
         [3 4 5])))

(deftest funcall-4
  (is (= (let [cc (atom nil)]
           [(with-call-cc
              (+ (let-cc k
                         (reset! cc k)
                         1)
                 2))
            (@cc 2)
            (@cc 3)])
         [1 4 5])))

(deftest funcall-5
  (is (= (let [cc (atom nil)]
           [(with-call-cc
              (+ 1
                 (let-cc k
                         (reset! cc k)
                         2)))
            (@cc 2)
            (@cc 3)])
         [2 3 4])))

(deftest funcall-6
  (is (= (let [cc (atom nil)]
           [(with-call-cc
              (+ (* 3
                    (let-cc k
                            (reset! cc k)
                            (k 1)))
                 2))
            (@cc 2)
            (@cc 3)])
         [5 8 11])))

(deftest funcall-7
  (is (= (with-call-cc ((fn [a b] (+ a b)) 1 2))
         3)))

(deftest funcall-8
  (is (= (with-call-cc (list (seq '())))
         '(nil))))

(deftest funcall-9
  (is (= (letfn [(some-vals [] [42 84])]
           (let [a (atom nil)
                 b (atom nil)]
             (with-call-cc
               (some-vals))))
         [42 84])))

;; quote
(deftest quote-1 
  (is (= (with-call-cc 'a) 'a)))

(deftest quote-2
  (is (= (with-call-cc '(a)) '(a))))

(deftest quote-3
  (is (= (let [cc (atom nil)]
           [(with-call-cc 
              (concat '(a b)
                      (let-cc k
                              (reset! cc k)
                              (k '(c)))))
            (@cc '(d))
            (@cc '(e))])
         ['(a b c) '(a b d) '(a b e)])))

;; do
(deftest do-1
  (is (= (with-call-cc (do 1 2 3))
         3)))

(deftest do-2
  (is (= (with-out-str
           (with-call-cc (do
                           (print 1)
                           (print 2)
                           (print 3))))
         "123")))

(deftest do-3
  (is (= (let [cc (atom nil)]
           [(with-call-cc (do
                            1 (let-cc k
                                      (reset! cc k)
                                      (k 2)) 3))
            (@cc 5)])
         [3 3])))

(deftest do-4
  (is (= (let [cc (atom nil)]
           [(with-call-cc (do
                            1 (let-cc k
                                      (reset! cc k)
                                      2) 3))
            (@cc 5)])
         [2 3])))

;; if
(deftest if-1
  (is (= (with-call-cc
           (if nil 1 2))
         2)))

(deftest if-2
  (is (= (with-call-cc
           (if true 1 2))
         1)))

(deftest if-3
  (is (= (let [cc (atom nil)]
           [(with-call-cc
              (if true 
                (let-cc k
                        (reset! cc k)
                        (k 1))
                2))
            (@cc 10)])
         [1 10])))

(deftest if-4
  (is (= (let [cc (atom nil)]
           (with-call-cc
             (if nil 
               (let-cc k
                       (reset! cc k)
                       (k 1)) 
               2))
           @cc)
         nil)))

(deftest if-5
  (is (= (let [cc (atom nil)]
           [(with-call-cc
              (if nil 1 (let-cc k
                                (reset! cc k)
                                (k 2))))
            (@cc 10)])
         [2 10])))

(deftest if-6
  (is (= (let [cc (atom nil)]
           (with-call-cc
             (if true 1 (let-cc k
                                (reset! cc k)
                                (k 2))))
           @cc)
         nil)))

(deftest if-7
  (is (= (let [cc (atom nil)]
           [(with-call-cc
              (if (let-cc k
                          (reset! cc k)
                          (k true))
                1 2))
            (@cc nil)])
         [1 2])))

;; function
(deftest function-1
  (is (= (let [f (with-call-cc +)]
           (f 1 2))
         3)))

(deftest function-2
  (is (= (let [f (with-call-cc (fn [a b] (+ a b)))]
           (f 1 2))
         3)))

;; function-3 need to understand how to convert

(deftest function-4
  (is (= (let [cc (atom nil)]
           [(with-call-cc
              (+ 1 ((fn [a b] (+ a b (let-cc k
                                             (reset! cc k)
                                             (k 0))))
                    1 2)))
            (@cc 1)
            (@cc 10)])
         [4 5 14])))

(deftest function-5
  (is (= (with-call-cc
           ((fn [a b]
              (+ a 1))
            5 10))
         6)))

(deftest function-6
  (is (= (let [cc (atom nil)]
           [((fn-cc [a]
                    (+ a (let-cc k
                                 (reset! cc k)
                                 (k 1))))
             10)
            (@cc 11)])
         [11 21])))

(deftest function-7
  (is (= (let [k (fn-cc [a] (+ a (let-cc k k (k 1))))]
           [(k 10) (k 11) (k 15)])
         [11 12 16])))

;; let
(deftest let-1
  (is (= (with-call-cc
           (let [a 1
                 b (+ a 1)]
             (+ a b)))
         3)))

(deftest let-2
  (is (= (with-call-cc
           (let []
             1))
         1)))

(deftest let-3
  (is (= (with-call-cc
           (let []))
         nil)))

(deftest let-4
  (is (= (let [cc (atom nil)]
           [(with-call-cc
              (let [a (let-cc k
                              (reset! cc k)
                              (k 1))
                    b (+ a 1)]
                (+ a b)))
            (@cc 2)
            (@cc 3)])
         [3 5 7])))

(deftest let-5
  (is (= (let [cc (atom nil)]
           [(with-call-cc
              (let [a 1
                    b (+ a 1)]
                (+ a b (let-cc k
                               (reset! cc k)
                               (k 10)))))
            (@cc 20)])
         [13 23])))

(deftest let-6
  (is (= (with-call-cc
           (let [a 1
                 b (+ a 1)
                 c nil]
             (list (+ a b) c)))
         '(3 nil))))

(deftest let-7
  (is (= (with-call-cc
           (let [a 1]
             (+ a 2)))
         3)))

(deftest let-8
  (is (= (with-call-cc
           (let [[x y] [1 2]]
             (+ x y)))
         3)))

(deftest let-9
  (is (= (let [cc (atom nil)]
           [(with-call-cc
              (let [[x y] (let-cc k
                                  (reset! cc k)
                                  (k [1 2]))]
                (+ x y)))
            (@cc [3 4])])
         [3 7])))

(deftest let-10
  (is (= (with-call-cc
           (let [{x :x y :y} {:x 1, :y 2}]
             (+ x y)))
         3)))

(deftest let-11
  (is (= (with-call-cc
           (let [{x :x y :y :as z} {:x 1 :y 2}]
             (+ x y)
             z))
         {:x 1 :y 2})))

;; if-let
(deftest if-let-1
  (is (= (with-call-cc
           (if-let [a 1]
             (+ a 2)))
         3)))

(deftest if-let-2
  (is (= (with-call-cc
           (if-let [a 1]
             1))
         1)))

(deftest if-let-3
  (is (= (with-call-cc
           (if-let [a nil]
             1
             2))
         2)))

(deftest if-let-4
  (is (= (let [cc (atom nil)]
           [(with-call-cc
              (if-let [a (let-cc k
                                 (reset! cc k)
                                 (k 1))]
                (+ a 2)))
            (@cc 2)
            (@cc 3)])
         [3 4 5])))

(deftest if-let-5
  (is (= (let [cc (atom nil)]
           [(with-call-cc
              (if-let [a 1]
                (let-cc k
                        (reset! cc k)
                        (k 1))
                1))
            (@cc 2)
            (@cc 3)])
         [1 2 3])))

(deftest if-let-6
  (is (= (let [cc (atom nil)]
           (with-call-cc
             (if-let [a 1]
               1
               (let-cc k
                       (reset! cc k)
                       (k 1))))
           @cc)
         nil)))

(deftest if-let-7
  (is (= (let [cc (atom nil)]
           (with-call-cc
             (if-let [a nil]
               (let-cc k
                       (reset! cc k)
                       (k 1)) 
               2))
           @cc)
         nil)))

(deftest if-let-8
  (is (= (let [cc (atom nil)]
           [(with-call-cc
              (if-let [a nil]
                2
                (let-cc k
                        (reset! cc k)
                        (k 1))))
            (@cc 2)
            (@cc 3)])
         [1 2 3])))

(deftest if-let-9
  (is (= (let [cc (atom nil)]
           [(with-call-cc
              (if-let [a (let-cc k
                                 (reset! cc k)
                                 (k true))]
                1 2))
            (@cc nil)])
         [1 2])))

;; letfn
;; we need this to verify letfn environment masking works
(defmacro a [i j k]
  `(+ ~i ~j ~k))

(deftest leftn-1
  (is (= (with-call-cc
           (letfn [(a [i j] (+ i j))
                   (b [i j] (* i j))]
             (+ (a 1 2) (b 3 4))))
         15)))

(deftest letfn-2
  (is (= (let [cc (atom nil)]
           [(with-call-cc
              (letfn [(a [i j] (+ i j))
                      (b [i j] (* i j))]
                (+ (a 1 (let-cc k
                                (reset! cc k)
                                (k 2)))
                   (b 3 4))))
            (@cc 3)])
         [15 16])))

(deftest letfn-3
  (is (= (let [cc (atom nil)]
           [(with-call-cc 
              (letfn [(a [i] (+ i (let-cc k
                                          (reset! cc k)
                                          (k 2))))
                      (b [i j]
                        (* i j))]
                (+ (a 1) (b 3 4))))
            (@cc 3)])
         [15 16])))

(deftest letfn-4
  (is (= ((with-call-cc
            (letfn [(a [i j] (+ i j))]
              (function a)))
          3 4)
         7)))

(deftest letfn-5
  (is (= (with-call-cc
           (letfn [(a [i j] (+ i j))]
             (letfn [(a [i j] (+ i j))]
               1)
             (a 1 2)))
         3)))

(deftest letfn-6
  (is (= (with-call-cc
           (letfn [(a [i j] 1)]
             1))
         1)))

(deftest letfn-7
  (is (= (with-call-cc
           (letfn [(a [i j] (+ i j))
                   (b [i j] (* (a i j) 3))]
             (+ (b 1 2))))
         9)))

(deftest letfn-8
  (is (= (let [cc (atom nil)]
           [(with-call-cc
              (letfn [(a [i j] (+ i j (let-cc k
                                              (reset! cc k)
                                              (k 0))))
                      (b [i j] (* (a i j) 3))]
                (+ (b 1 2))))
            (@cc 1)])
         [9 12])))

(deftest letfn-9
  (is (= (let [cc (atom nil)]
           [(with-call-cc
              (letfn [(a [i j] (+ i j))
                      (b [i j] (* (a i j) 3))]
                (+ (b 1 2) (let-cc k
                                   (reset! cc k)
                                   (k 0)))))
            (@cc 1)])
         [9 10])))

(deftest letfn-10
  (is (= ((with-call-cc
            (letfn [(a [i j] (+ i j))
                    (b [] (function a))]
              (b)))
          1 2)
         3)))

;; nested with-call-cc
(deftest with-call-cc-nested
  (is (= (let [cc (atom nil)]
           [(with-call-cc
              (+ 1 (with-call-cc (let-cc k
                                         (reset! cc k)
                                         (k 2)))))
            (@cc 4)])
         [3 5])))

;; without-call-cc
(deftest without-call-cc-1
  (is (= (let [cc (atom nil)]
           [(with-call-cc
              1 (without-call-cc 2 3) (let-cc k
                                              (reset! cc k)
                                              (k 4)))
            (@cc 10)])
         [4 10])))

(deftest without-call-cc-2
  (is (= (without-call-cc 1 2 3)
         3)))

;; defn
(defn-cc test-fn-cc-1 [a b]
  (+ a b))

(deftest defn-cc-1
  (is (= (test-fn-cc-1 1 2)
         3)))

(deftest defn-cc-2
  (is (= (with-call-cc (+ 1 (test-fn-cc-1 1 2)))
         4)))

(def test-fn-cc (atom nil))

(defn-cc test-fn-cc-2 [a b]
  (+ a (let-cc k
               (reset! test-fn-cc k)
               (k b))))

(deftest defn-cc-3
  (is (= [(test-fn-cc-2 1 2)
          (@test-fn-cc 10)]
           [3 11])))

(defn-cc test-fn-cc-3 [a]
  1)

(deftest defn-cc-4
  (is (= (test-fn-cc-3 10)
         1)))

;; explicit apply
(defn-cc add-cc-test [a b]
  (+ a b))

(deftest explicit-apply-1
  (is (= (let [cc (atom nil)]
           [(with-call-cc (+ 1 (apply (fn [a]
                                        (+ (let-cc k
                                                   (reset! cc k)
                                                   (k 1)) a)) (list 5))))
            (@cc 2)])
         [7 8])))

(deftest explicit-apply-2
  (is (= (let [cc (atom nil)]
           [(with-call-cc (+ 1 (apply (fn [a b c]
                                        (+ (let-cc k
                                                   (reset! cc k)
                                                   (k 1))
                                           a b c))
                                      3 4 (list 5))))
            (@cc 2)])
         [14 15])))

(deftest explicit-apply-3
  (is (= (with-call-cc (apply + 1 2 3 (list 4 5)))
         15)))

(deftest explicit-apply-4
  (is (= (with-call-cc (apply add-cc-test 3 (list 4)))
         7)))

;; ref/atom
(deftest atom-1
  (is (= @(with-call-cc (atom nil))
         @(atom nil))))

(deftest ref-1
  (is (= @(with-call-cc (ref {}))
         @(ref {}))))