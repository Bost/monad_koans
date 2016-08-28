#_(use 'clojure.algo.monads)

(defn mf-a [x]
  (fn [c]
    (c (inc x))))

(defn mf-b [x]
  (fn [c]
    (c (* 2 x))))

(defn mf-c [x]
  (fn [c]
    (c (dec x))))

(defn f-a [x]
  (inc x))

(defn f-b [x]
  (* 2 x))

(defn f-c [x]
  (dec x))

(def mm-result
  (fn mm-result-cont [v]
    (fn [c]
      (c v))))

(defn square [x] (* x x))

(defn f1 [x] [(dec x) x (inc x)]) ;; (f1 5) => [4 5 6]

(defn f2 [x] [x (square x)])      ;; (f2 5) => [5 25]


;; mm-bind:
;; Accept two params, a monadic value (mv) and a monadic function (mf)
;; Return a monadic value which is a function that accepts a continuation and calls it with some value.
;; Unwrap mv to get at the inner value. This must be done by calling mv and passing it a continuation.
;; The continuation to be passed to mv must be a combination of mf and the outer continuation passed to the monadic value returned by mm-bind.
;; So this inner continuation must be created when the monadic value returned by mm-bind is called with the outer continuation.
;; This inner continuation must be a function that accepts a value (which comes from inside mv) and calls mf with this value which results in...
;; A function that accepts a continuation which must immediately be called with the outer continuation.

(def mm-bind
  (fn mm-bind-cont [mv mf]
    (fn [c]
      (mv (fn [v]
            ((mf v) c))))))
#_(meditations
 "bla"
 (= [1 2 3 3 4 5]
    ((with-monad sequence-m (m-chain [f2 f1])) 2)))

((mm-result 21) identity) ;; => 21

((mm-bind (mm-result 21) mf-a) identity) ;; => 22

;; Following:
((mm-bind (mm-bind (mm-result 21) mf-a) mf-b) identity) ;; => 44

;; should be equivalent to:
((mm-bind (mm-result 21) (with-monad cont-m
                           (m-chain [mf-a mf-b]))) identity)

((mm-bind (mm-bind (mm-bind (mm-result 21) mf-a) mf-b) mf-c) identity) ;; => 43
