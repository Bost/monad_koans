(defproject monad-koans "1.0.0"
  :description "Set of exercises (koans) to learn monads in clojure"
  :dependencies
  [[org.clojure/clojure "1.10.1"]
   [com.rpl/specter "1.1.2"]
   [koan-engine "0.2.5"]
   [org.clojure/algo.monads "0.1.6"]
   [org.clojure/core.logic "0.8.11"]]
  :dev-dependencies [[lein-koan "0.1.3"]]
  :plugins [[lein-koan "0.1.5"]])
