(defproject delimc "0.2.0"
  :description "Delimited Continuations for Clojure"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.339"]]

  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-shell     "0.5.0"]]

  :clean-targets ^{:protect false} [:target-path "resources"]

  :shell          {:commands {"open" {:windows ["cmd" "/c" "start"]
                                      :macosx  "open"
                                      :linux   "xdg-open"}}}

  :cljsbuild
    {:builds [{:id "prod"
               :source-paths ["src"]
               :compiler
                 {:main delimc.core
                  :output-to "resources/public/js/delimc.js"
                  :output-dir "resources/public/js/out"
                  :target :nodejs
                  :optimizations :simple
                  :pretty-print true }}
              {:id "test"
               :source-paths ["src" "test"]
               :compiler
                 {:main delimc.test.core-test
                  :output-to "resources/private/js/cljs-test.js"
                  :output-dir "resources/private/js/out"
                  :target :nodejs
                  :optimizations :simple
                  :pretty-print true }}
              ]
     }

  :aliases {"cljs-test" ["do" 
                          ["clean"] 
                          ["cljsbuild" "once" "test"]
                          ["shell" "node" "resources/private/js/cljs-test.js"]]}
)
