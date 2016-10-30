(ns koans.5-specter
  (:require [koan-engine.core :refer :all]
            [com.rpl.specter :as sp]))

(def data {:a [{:aa 1 :bb 2}
               {:cc 3}]
           :b [{:dd 4}]})

(meditations
 "Basic conteplations: Truth has no intro rule"
 (= true true)

 "Basic conteplations: Falsity has no elim rule"
 (= (not true) false)

 (= ;; Manual Clojure
  (let [map-vals (fn [m afn]
                   (->> m (map (fn [[k v]] [k (afn v)])) (into {})))]
    (map-vals data
              (fn [v]
                (mapv
                 (fn [m]
                   (map-vals
                    m
                    (fn [v] (if (even? v) (inc v) v))))
                 v))))

  ;; Specter
  (sp/transform [sp/MAP-VALS sp/ALL sp/MAP-VALS even?] inc data)
  ))
