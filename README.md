# delimc

A [delimited continuations](https://en.wikipedia.org/wiki/Delimited_continuation) library for Clojure/Script.

Portions based on cl-cont by Slava Akhmechet (http://defmacro.org).

## Usage

```clojure
(require '[delimc.core :refer [reset shift]])

(defmacro amb [xs]
  `(shift k# (mapcat k# ~xs)))

(reset
 (let [x (amb [1 2 3])
       y (amb [2 4 6])]
    (if (zero? (mod (+ x y) 3))
      [[x y]])))
;; => ([1 2] [2 4] [3 6])
```

## Test

### Clojure

```
$ lein test
```

### ClojureScript

```
$ lein cljs-test
```

## References

* [Literate Engines in Lisp](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.45.6198)
* [Threads Yield Continuations](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.41.4786)
* [Yield: Mainstream Delimited Continuations](http://parametricity.net/dropbox/yield.subc.pdf)
* [Composable Continuations Tutorial](http://community.schemewiki.org/?composable-continuations-tutorial)
* [A Monadic Framework for Subcontinuations](http://research.microsoft.com/en-us/um/people/simonpj/papers/control/control.pdf)
* [Subcontinuations](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.63.1754&rep=rep1&type=pdf)
* [Models of Control and Their Implicatations for Programming Languages](www.ccs.neu.edu/racket/pubs/thesis-sitaram.ps.gz)
* [Monads and Composable Continuations](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.94.985&rep=rep1&type=pdf)
