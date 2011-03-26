(ns delimc.test.core
  (:use [delimc.core] :reload)
  (:use [clojure.test]))

;; not-seq
(deftest not-seq-1
  (is (= (reset 1) 1)))

(deftest not-seq-2
  (is (= (let [cc (atom nil)]
           [(reset 
             (shift k
                    (reset! cc k)
                    (@cc 1)))
            (@cc 2)])
         [1 2])))

;; funcall
(deftest funcall-1 
  (is (= (reset (+ 1 2)) 
         3)))

(deftest funcall-2
  (is (= (reset (- (+ 1 2) 1))
         2)))

(deftest funcall-3
  (is (= (let [cc (atom nil)]
           [(reset
             (+ (shift k
                       (reset! cc k)
                       (k 1))
                2))
            (@cc 2)
            (@cc 3)])
         [3 4 5])))

(deftest funcall-4
  (is (= (let [cc (atom nil)]
           [(reset
             (+ (shift k
                       (reset! cc k)
                       1)
                2))
            (@cc 2)
            (@cc 3)])
         [1 4 5])))

(deftest funcall-5
  (is (= (let [cc (atom nil)]
           [(reset
             (+ 1
                (shift k
                       (reset! cc k)
                       2)))
            (@cc 2)
            (@cc 3)])
         [2 3 4])))

(deftest funcall-6
  (is (= (let [cc (atom nil)]
           [(reset
             (+ (* 3
                   (shift k
                          (reset! cc k)
                          (k 1)))
                2))
            (@cc 2)
            (@cc 3)])
         [5 8 11])))

(deftest funcall-7
  (is (= (reset ((fn [a b] (+ a b)) 1 2))
         3)))

(deftest funcall-8
  (is (= (reset (list (seq '())))
         '(nil))))

(deftest funcall-9
  (is (= (letfn [(some-vals [] [42 84])]
           (let [a (atom nil)
                 b (atom nil)]
             (reset
              (some-vals))))
         [42 84])))

;; quote
(deftest quote-1 
  (is (= (reset 'a) 'a)))

(deftest quote-2
  (is (= (reset '(a)) '(a))))

(deftest quote-3
  (is (= (let [cc (atom nil)]
           [(reset 
             (concat '(a b)
                     (shift k
                            (reset! cc k)
                            (k '(c)))))
            (@cc '(d))
            (@cc '(e))])
         ['(a b c) '(a b d) '(a b e)])))

;; do
(deftest do-1
  (is (= (reset (do 1 2 3))
         3)))

(deftest do-2
  (is (= (with-out-str
           (reset (do
                    (print 1)
                    (print 2)
                    (print 3))))
         "123")))

(deftest do-3
  (is (= (let [cc (atom nil)]
           [(reset (do
                     1 (shift k
                              (reset! cc k)
                              (k 2)) 3))
            (@cc 5)])
         [3 3])))

(deftest do-4
  (is (= (let [cc (atom nil)]
           [(reset (do
                     1 (shift k
                              (reset! cc k)
                              2) 3))
            (@cc 5)])
         [2 3])))

;; if
(deftest if-1
  (is (= (reset
          (if nil 1 2))
         2)))

(deftest if-2
  (is (= (reset
          (if true 1 2))
         1)))

(deftest if-3
  (is (= (let [cc (atom nil)]
           [(reset
             (if true 
               (shift k
                      (reset! cc k)
                      (k 1))
               2))
            (@cc 10)])
         [1 10])))

(deftest if-4
  (is (= (let [cc (atom nil)]
           (reset
            (if nil 
              (shift k
                     (reset! cc k)
                     (k 1)) 
              2))
           @cc)
         nil)))

(deftest if-5
  (is (= (let [cc (atom nil)]
           [(reset
             (if nil 1 (shift k
                              (reset! cc k)
                              (k 2))))
            (@cc 10)])
         [2 10])))

(deftest if-6
  (is (= (let [cc (atom nil)]
           (reset
            (if true 1 (shift k
                              (reset! cc k)
                              (k 2))))
           @cc)
         nil)))

(deftest if-7
  (is (= (let [cc (atom nil)]
           [(reset
             (if (shift k
                        (reset! cc k)
                        (k true))
               1 2))
            (@cc nil)])
         [1 2])))

;; function
(deftest function-1
  (is (= (let [f (reset +)]
           (f 1 2))
         3)))

(deftest function-2
  (is (= (let [f (reset (fn [a b] (+ a b)))]
           (f 1 2))
         3)))

;; function-3 need to understand how to convert

(deftest function-4
  (is (= (let [cc (atom nil)]
           [(reset
             (+ 1 ((fn [a b] (+ a b (shift k
                                           (reset! cc k)
                                           (k 0))))
                   1 2)))
            (@cc 1)
            (@cc 10)])
         [4 5 14])))

(deftest function-5
  (is (= (reset
          ((fn [a b]
             (+ a 1))
           5 10))
         6)))

(deftest function-6
  (is (= (let [cc (atom nil)]
           [((fn-cc [a]
                    (+ a (shift k
                                (reset! cc k)
                                (k 1))))
             10)
            (@cc 11)])
         [11 21])))

(deftest function-7
  (is (= (let [k (fn-cc [a] (+ a (shift k k (k 1))))]
           [(k 10) (k 11) (k 15)])
         [11 12 16])))

;; let
(deftest let-1
  (is (= (reset
          (let [a 1
                b (+ a 1)]
            (+ a b)))
         3)))

(deftest let-2
  (is (= (reset
          (let []
            1))
         1)))

(deftest let-3
  (is (= (reset
          (let []))
         nil)))

(deftest let-4
  (is (= (let [cc (atom nil)]
           [(reset
             (let [a (shift k
                            (reset! cc k)
                            (k 1))
                   b (+ a 1)]
               (+ a b)))
            (@cc 2)
            (@cc 3)])
         [3 5 7])))

(deftest let-5
  (is (= (let [cc (atom nil)]
           [(reset
             (let [a 1
                   b (+ a 1)]
               (+ a b (shift k
                             (reset! cc k)
                             (k 10)))))
            (@cc 20)])
         [13 23])))

(deftest let-6
  (is (= (reset
          (let [a 1
                b (+ a 1)
                c nil]
            (list (+ a b) c)))
         '(3 nil))))

(deftest let-7
  (is (= (reset
          (let [a 1]
            (+ a 2)))
         3)))

(deftest let-8
  (is (= (reset
          (let [[x y] [1 2]]
            (+ x y)))
         3)))

(deftest let-9
  (is (= (let [cc (atom nil)]
           [(reset
             (let [[x y] (shift k
                                (reset! cc k)
                                (k [1 2]))]
               (+ x y)))
            (@cc [3 4])])
         [3 7])))

(deftest let-10
  (is (= (reset
          (let [{x :x y :y} {:x 1, :y 2}]
            (+ x y)))
         3)))

(deftest let-11
  (is (= (reset
          (let [{x :x y :y :as z} {:x 1 :y 2}]
            (+ x y)
            z))
         {:x 1 :y 2})))

;; if-let
(deftest if-let-1
  (is (= (reset
          (if-let [a 1]
            (+ a 2)))
         3)))

(deftest if-let-2
  (is (= (reset
          (if-let [a 1]
            1))
         1)))

(deftest if-let-3
  (is (= (reset
          (if-let [a nil]
            1
            2))
         2)))

(deftest if-let-4
  (is (= (let [cc (atom nil)]
           [(reset
             (if-let [a (shift k
                               (reset! cc k)
                               (k 1))]
               (+ a 2)))
            (@cc 2)
            (@cc 3)])
         [3 4 5])))

(deftest if-let-5
  (is (= (let [cc (atom nil)]
           [(reset
             (if-let [a 1]
               (shift k
                      (reset! cc k)
                      (k 1))
               1))
            (@cc 2)
            (@cc 3)])
         [1 2 3])))

(deftest if-let-6
  (is (= (let [cc (atom nil)]
           (reset
            (if-let [a 1]
              1
              (shift k
                     (reset! cc k)
                     (k 1))))
           @cc)
         nil)))

(deftest if-let-7
  (is (= (let [cc (atom nil)]
           (reset
            (if-let [a nil]
              (shift k
                     (reset! cc k)
                     (k 1)) 
              2))
           @cc)
         nil)))

(deftest if-let-8
  (is (= (let [cc (atom nil)]
           [(reset
             (if-let [a nil]
               2
               (shift k
                      (reset! cc k)
                      (k 1))))
            (@cc 2)
            (@cc 3)])
         [1 2 3])))

(deftest if-let-9
  (is (= (let [cc (atom nil)]
           [(reset
             (if-let [a (shift k
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
  (is (= (reset
          (letfn [(a [i j] (+ i j))
                  (b [i j] (* i j))]
            (+ (a 1 2) (b 3 4))))
         15)))

(deftest letfn-2
  (is (= (let [cc (atom nil)]
           [(reset
             (letfn [(a [i j] (+ i j))
                     (b [i j] (* i j))]
               (+ (a 1 (shift k
                              (reset! cc k)
                              (k 2)))
                  (b 3 4))))
            (@cc 3)])
         [15 16])))

(deftest letfn-3
  (is (= (let [cc (atom nil)]
           [(reset 
             (letfn [(a [i] (+ i (shift k
                                        (reset! cc k)
                                        (k 2))))
                     (b [i j]
                       (* i j))]
               (+ (a 1) (b 3 4))))
            (@cc 3)])
         [15 16])))

(deftest letfn-4
  (is (= ((reset
           (letfn [(a [i j] (+ i j))]
             (function a)))
          3 4)
         7)))

(deftest letfn-5
  (is (= (reset
          (letfn [(a [i j] (+ i j))]
            (letfn [(a [i j] (+ i j))]
              1)
            (a 1 2)))
         3)))

(deftest letfn-6
  (is (= (reset
          (letfn [(a [i j] 1)]
            1))
         1)))

(deftest letfn-7
  (is (= (reset
          (letfn [(a [i j] (+ i j))
                  (b [i j] (* (a i j) 3))]
            (+ (b 1 2))))
         9)))

(deftest letfn-8
  (is (= (let [cc (atom nil)]
           [(reset
             (letfn [(a [i j] (+ i j (shift k
                                            (reset! cc k)
                                            (k 0))))
                     (b [i j] (* (a i j) 3))]
               (+ (b 1 2))))
            (@cc 1)])
         [9 12])))

(deftest letfn-9
  (is (= (let [cc (atom nil)]
           [(reset
             (letfn [(a [i j] (+ i j))
                     (b [i j] (* (a i j) 3))]
               (+ (b 1 2) (shift k
                                 (reset! cc k)
                                 (k 0)))))
            (@cc 1)])
         [9 10])))

(deftest letfn-10
  (is (= ((reset
           (letfn [(a [i j] (+ i j))
                   (b [] (function a))]
             (b)))
          1 2)
         3)))

;; nested shift
(deftest shift-nested
  (is (= (let [cc (atom nil)]
           [(reset
             (+ 1 (reset (shift k
                                (reset! cc k)
                                (k 2)))))
            (@cc 4)])
         [3 5])))

;; unreset
(deftest unreset-1
  (is (= (let [cc (atom nil)]
           [(reset
             1 (unreset 2 3) (shift k
                                    (reset! cc k)
                                    (k 4)))
            (@cc 10)])
         [4 10])))

(deftest unreset-2
  (is (= (unreset 1 2 3)
         3)))

;; explicit apply
(deftest explicit-apply-1
  (is (= (let [cc (atom nil)]
           [(reset (+ 1 (apply (fn [a]
                                 (+ (shift k
                                           (reset! cc k)
                                           (k 1)) a)) (list 5))))
            (@cc 2)])
         [7 8])))

(deftest explicit-apply-2
  (is (= (let [cc (atom nil)]
           [(reset (+ 1 (apply (fn [a b c]
                                 (+ (shift k
                                           (reset! cc k)
                                           (k 1))
                                    a b c))
                               3 4 (list 5))))
            (@cc 2)])
         [14 15])))

(deftest explicit-apply-3
  (is (= (reset (apply + 1 2 3 (list 4 5)))
         15)))

;; ref/atom
(deftest atom-1
  (is (= @(reset (atom nil))
         @(atom nil))))

(deftest ref-1
  (is (= @(reset (ref {}))
         @(ref {}))))