delimc
----

A delimited continuations library for Clojure 1.4.0 (and 1.3.0). Portions based on cl-cont by Slava Akhmechet (http://defmacro.org).

```clj
(def cont1 (atom nil))
(def cont2 (atom nil))
(def cont3 (atom nil))
(def cont4 (atom nil))

(reset (+ 1 (apply (fn [a b c]
                     (+ (shift k
                          (reset! cont1 k)
                            (k 1))
                         a b c))
                    3 4 (list 5)))) ;; 14

(@cont1 2) ;; 15

(reset
  (+ 1 (reset (shift k
                (reset! cont2 k)
                (k 2)))
       (reset (shift k
                (reset! cont3 k)
                (k 3))))) ;; 6

(@cont2 4) ;; 8
(@cont3 10) ;; 15

(reset (str "Hello" (shift k
                      (reset! cont4 k)
                        (k ", today is "))
            "a nice day!")) ; "Hello, today is a nice day"

(@cont4 ", yesterday was ") ; "Hello, yesterday was a nice day"
```

Test
----

To test for clojure by Leiningen:

```
$ lein test
```

To test for clojurescript by Leiningen:

```
$ lein cljs-test
```

References
----

* [Literate Engines in Lisp](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.45.6198)
* [Threads Yield Continuations](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.41.4786)
* [Yield: Mainstream Delimited Continuations](http://parametricity.net/dropbox/yield.subc.pdf)
* [Composable Continuations Tutorial](http://community.schemewiki.org/?composable-continuations-tutorial)
* [A Monadic Framework for Subcontinuations](http://research.microsoft.com/en-us/um/people/simonpj/papers/control/control.pdf)
* [Subcontinuations](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.63.1754&rep=rep1&type=pdf)
* [Models of Control and Their Implicatations for Programming Languages](www.ccs.neu.edu/racket/pubs/thesis-sitaram.ps.gz)
* [Monads and Composable Continuations](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.94.985&rep=rep1&type=pdf)
