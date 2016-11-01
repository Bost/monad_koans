(ns koans.6-specter
  (:require [koan-engine.core :refer :all]
            [com.rpl.specter :as sp]))

(def data
  {:a [{:aa 1 :bb 2} {:cc 3}] :b [{:dd 4}]})

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

  "In the beginng was a theory"
  (=
   {:a [:big :bang 1 2 3]} #_{:a [_ _ 1 2 3]}
   (sp/setval [:a sp/BEGINNING] [:big :bang] {:a [1 2 3]}))

  "However the end still lies ahead"
  (=
   {:a [1 2 3 Double/POSITIVE_INFINITY]} #_{:a [1 2 3 _]}
   (sp/setval [:a sp/END] [Double/POSITIVE_INFINITY] {:a [1 2 3]}))

  "Rise and shine"
  (=
   {:a {:aa 2} :b {:ba 0, :bb 3} :inc {:ris 3} :and {:shy 9}}
   #_{:a {:aa 2} :b {:ba 0, :bb 3} :inc {:ris _} :and {:shy _}}
   (sp/transform [sp/MAP-VALS sp/MAP-VALS]
                 inc
                 {:a {:aa 1}
                  :b {:ba -1 :bb 2}
                  :inc {:ris 2} :and {:shy 8}}))

  "Select only those vals for whose the #(..) returns true
   all in all, it's not so bad"
  (=
   [3 3 18 6 12]
   (sp/select [sp/ALL sp/ALL #(= 0 (mod % 3))]
              [[1  2  3  4]
               [          ]
               [5  3  2 18]
               [2  4  6   ]
               [12        ]]))
   )
