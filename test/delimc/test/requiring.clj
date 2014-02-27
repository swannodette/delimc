(ns delimc.test.requiring
  (:require [delimc.core :as d])
  (:use [clojure.test]))

;; not-seq
(deftest not-seq-1
  (is (= (d/reset 1) 1)))

(deftest not-seq-2
  (is (= (let [cc (atom nil)]
           [(d/reset
             (d/shift k
                    (reset! cc k)
                    (@cc 1)))
            (@cc 2)])
         [1 2])))

(deftest funcall-7
  (is (= (d/reset ((fn [a b] (+ a b)) 1 2))
         3)))

(deftest funcall-8
  (is (= (d/reset (list (seq '())))
         '(nil))))

(deftest funcall-9
  (is (= (letfn [(some-vals [] [42 84])]
           (let [a (atom nil)
                 b (atom nil)]
             (d/reset
              (some-vals))))
         [42 84])))

;; do
(deftest do-1
  (is (= (d/reset (do 1 2 3))
         3)))

;; if
(deftest if-1
  (is (= (d/reset
           (if nil 1 2))
         2)))

(deftest function-4
  (is (= (let [cc (atom nil)]
           [(d/reset
             (+ 1 ((fn [a b] (+ a b (d/shift k
                                           (reset! cc k)
                                           (k 0))))
                   1 2)))
            (@cc 1)
            (@cc 10)])
         [4 5 14])))

(deftest function-5
  (is (= (d/reset
          ((fn [a b]
             (+ a 1))
           5 10))
         6)))

(deftest function-6
  (is (= (let [cc (atom nil)]
           [((d/fn-cc [a]
                    (+ a (d/shift k
                                (reset! cc k)
                                (k 1))))
             10)
            (@cc 11)])
         [11 21])))

(deftest function-7
  (is (= (let [k (d/fn-cc [a] (+ a (d/shift k k (k 1))))]
           [(k 10) (k 11) (k 15)])
         [11 12 16])))

;; let
(deftest let-1
  (is (= (d/reset
          (let [a 1
                b (+ a 1)]
            (+ a b)))
         3)))

;; letfn
;; we need this to verify letfn environment masking works
(defmacro a [i j k]
  `(+ ~i ~j ~k))

(deftest leftn-1
  (is (= (d/reset
          (letfn [(a [i j] (+ i j))
                  (b [i j] (* i j))]
            (+ (a 1 2) (b 3 4))))
         15)))

(deftest letfn-4
  (is (= ((d/reset
           (letfn [(a [i j] (+ i j))]
             (d/function a)))
          3 4)
         7)))

(deftest letfn-10
  (is (= ((d/reset
           (letfn [(a [i j] (+ i j))
                   (b [] (d/function a))]
             (b)))
          1 2)
         3)))

;; nested shift
(deftest shift-nested
  (is (= (let [cc (atom nil)]
           [(d/reset
             (+ 1 (d/reset (d/shift k
                                (reset! cc k)
                                (k 2)))))
            (@cc 4)])
         [3 5])))

;; unreset
(deftest unreset-1
  (is (= (let [cc (atom nil)]
           [(d/reset
             1 (d/unreset 2 3) (d/shift k
                                    (reset! cc k)
                                    (k 4)))
            (@cc 10)])
         [4 10])))

(deftest unreset-2
  (is (= (d/unreset 1 2 3)
         3)))

;; explicit apply
(deftest explicit-apply-1
  (is (= (let [cc (atom nil)]
           [(d/reset (+ 1 (apply (fn [a]
                                 (+ (d/shift k
                                           (reset! cc k)
                                           (k 1)) a)) (list 5))))
            (@cc 2)])
         [7 8])))

(deftest explicit-apply-2
  (is (= (let [cc (atom nil)]
           [(d/reset (+ 1 (apply (fn [a b c]
                                 (+ (d/shift k
                                           (reset! cc k)
                                           (k 1))
                                    a b c))
                               3 4 (list 5))))
            (@cc 2)])
         [14 15])))

(deftest explicit-apply-3
  (is (= (d/reset (apply + 1 2 3 (list 4 5)))
         15)))
