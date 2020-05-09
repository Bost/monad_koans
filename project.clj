(defproject monad-koans :lein-v
  :description "Set of exercises (koans) to learn monads in clojure"
  :dependencies
  [
   [org.clojure/clojure "1.10.1"]
   [com.rpl/specter "1.1.3"]
   [koan-engine "0.2.5"]
   [org.clojure/algo.monads "0.1.6"]
   [org.clojure/core.logic "1.0.0"]]
  :dev-dependencies
  [
   [lein-koan "0.1.3"]]
  :plugins
  [
   [lein-koan "0.1.5"]
   [com.roomkey/lein-v "7.2.0"]])
