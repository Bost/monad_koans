(ns koans.6-specter
  (:require [koan-engine.core :refer :all]
            [com.rpl.specter :as sp]))

(def data
  {:a [{:aa 1 :bb 2} {:cc 3}] :b [{:dd 4}]})

(def max-3 (- Integer/MAX_VALUE 3))
(def max-2 (- Integer/MAX_VALUE 2))
(def max-1 (- Integer/MAX_VALUE 1))
(def max-0 (- Integer/MAX_VALUE 0))

(meditations
 "Basic conteplations: Truth has no intro rule"
 (= true true)

 "Basic conteplations: Falsity has no elim rule"
 (= (not true) false)

 "Increment all even integers in every submap"
 (=
  {:a [{:aa 1 :bb 3} {:cc 3}] :b [{:dd 5}]}
  ;; Specter
  (sp/transform [sp/MAP-VALS sp/ALL sp/MAP-VALS even?] inc data)
  ;; Manual Clojure
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

  "In the beginng there was a theory"
  (=
   {:a [:big :bang 0 1 2]} #_{:a [_ _ 0 1 2]}
   (sp/setval [:a sp/BEGINNING] [:big :bang] {:a [0 1 2]}))

  "However the end lies quite far in the distance"
  (=
   {:a [max-3 max-2 max-1 max-0]} #_{:a [max-3 max-2 max-1 _]}
   (sp/setval [:a sp/END] [max-0] {:a [max-3 max-2 max-1]}))

  "Go, rise and shine! Long live the specter!"
  (=
   {:long {:liv 3}, :the {:specte 2}, :go {:ris 3}, :and {:shy 9}}
   #_{:long {:liv _}, :the {:specte _}, :go {:ris _}, :and {:shy _}}
   (sp/transform [sp/MAP-VALS sp/MAP-VALS]
                 inc
                 {:long {:liv 2} :the {:specte 1}
                  :go {:ris 2} :and {:shy 8}}))

  "All in all, we all bet on just a few lucky numbers"
  (=
   [3 3 5 7 3 34 12] #_[_ _ _ _ _ _ _]
   (sp/select [sp/ALL sp/ALL (fn lucky? [n] (some #(= n %) #{3 5 7 12 34}))]
              [[ 3   2   3   4]
               [              ]
               [ 5   7   3  18]
               [ 2  34   0    ]
               [    12        ]]))
   )
