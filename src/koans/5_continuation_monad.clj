(ns koans.5-continuation-monad
  "Continuations are for control flow - function call/return, exception
  handling, gotos, etc."
  (:use [clojure.algo.monads]))

(defmacro dbg [body]
  `(let [x# ~body]
     (println "dbg:" '~body "=" x#)
     x#))

(defn f-a [x] (inc x))
(defn f-b [x] (* 2 x))
(defn f-c [x] (dec x))

(defn mm-result
  "Accepts one parameter `a` (plain value).
  Returns a continuation-function `c` (monadic value). The `c` is executed over
  the `v`.
  Type signature: a -> m a
  ((fn [a] (fn [c] (c a))) 0) -> (fn [c] (c 0))"
  [a]
  (fn [c]
    (c a)))

(def mm-zero
  #_(mm-result identity)
  (mm-result 0))

(defn mm-plus [ma mb]
  (let [a (ma identity)
        b (mb identity)]
    (mm-result
     #_(comp a b) ;; `a` after `b`
     (+ a b))))

(defn mm-bind
  "Accepts two parameters: a monadic value `mv` and a monadic function `mf`.
  Returns a monadic value resulting from applying `mv` over the `v` extracted
  from the monadic container.
  Type signature: m a -> (a -> m b) -> m b

  `mf` is the continuation, i.e. the next function to execute."
  [mv mf]
  ;; from http://www.clojure.net/2012/03/24/Continuation-monad/
  ;; `mm-bind` must do the following:
  ;; - Accept two parameters, a monadic value `mv` and a monadic function `mf`
  ;; - Return a monadic value `(fn [c] ...)` which is a function that accepts a continuation `c` and calls it with some value.
  ;; - Unwrap `mv` to get at the inner value. This must be done by calling `mv` and passing it a continuation `(fn [v] ...)`, i.e. `(mv (fn [v] ...))`
  ;; - The (inner) continuation to be passed to `mv` must be a combination of `mf` and the outer continuation passed to the monadic value returned by `mm-bind`.
  ;; - So this inner continuation must be created when the monadic value returned by `mm-bind` is called with the outer continuation.
  ;; - This inner continuation must be a function that accepts a value (which comes from inside `mv`) and calls `mf` with this value which results in...
  ;; - A function that accepts a continuation which must immediately be called with the outer continuation.

  ;; `mv` is a monadic value, i.e. a plain value `v` inside a container. The
  ;; container is a function `(fn [c] (c v))`. In other words: the plain value
  ;; `v` sits inside some container / function / functional container `(fn ...)`
  ;; screened behind the call `(c v)` of some function `c` over the `v`, also
  ;; known as `(c v)` transformation. This transformation `c` is also a
  ;; parameter of the functional container `(fn [c] ...)`.
  (fn [c]
    (mv (fn [v]
          ((mf v) ;; a -> m b
           c))))
  #_(do
    (printf "mv: %s; mf: %s\n" (:name (meta mv)) (:name (meta mf)))
    (dbg
     (fn [c]
       ((dbg mv)
        (fn [v]
          (dbg (
                (dbg ((dbg mf) (dbg v)))
                (dbg c)))))))))

#_((def m4
   (mm-bind (def m3
              (mm-bind (def m2
                         (mm-bind
                          (def m1
                            (mm-bind (defn m0 [cmv] (cmv 0))
                                     (defn mHalt [x] (defn cHalt [c] x))))
                          (defn mInc2 [y] (defn cInc2 [c] (c (+ 2 y))))))
                       (defn mInc3 [y] (defn cInc3 [c] (c (+ 3 y))))))
            (defn mInc4 [y] (defn cInc4 [c] (c (+ 4 y))))))
 (dbg (fn extractor [x] (+ 10 x))))

;; 1. monadic law
(= ((mm-bind (mm-result 1) mf-a) identity)
   ((mf-a 1) identity))

;; 2. monadic law
(let [mv (fn [c] 42)
      #_(mm-result 42)]
  (= ((mm-bind mv mm-result) identity)
     (mv identity)))

;; 3. monadic law
(let [mv
      (fn [c] 42)
      #_(mm-result 42)]
  (= ((mm-bind (mm-bind mv mf-a) mf-b) identity)
     ((mm-bind mv (fn [x] (mm-bind (mf-a x) mf-b))) identity)))

((mm-result 21) identity) ;; => 21

((mm-bind (mm-result 21) mf-a) identity) ;; => 22

;; this equality must be true
(= ((mm-bind (mm-bind (mm-result 21) mf-a) mf-b) identity) ;; => 44
   ((mm-bind (mm-result 21) (with-monad cont-m
                              (m-chain [mf-a mf-b]))) identity))

((mm-bind (mm-bind (mm-bind (mm-result 21) mf-a) mf-b) mf-c) identity) ;; => 43

(defn mf-a [x]
  (let [fname "mf-a"]
    (printf "Starting %s ...\n" fname)
    (fn [c]
      (printf "%s started. Calculating ...\n" fname)
      (let [r (c (inc x))]
        (printf "%s calculated.\n" fname)
        r))))

(defn mf-b [x]
  (let [fname "mf-b"]
    (printf "Starting %s ...\n" fname)
    (fn [c]
      (printf "%s started. Calculating ...\n" fname)
      (let [r (c (* 2 x))]
        (printf "%s calculated.\n" fname)
        r))))

(defn mf-c [x]
  (let [fname "mf-c"]
    (printf "Starting %s ...\n" fname)
    (fn [c]
      (printf "%s started. Calculating ...\n" fname)
      (let [r (c (dec x))]
        (printf "%s calculated.\n" fname)
        r))))

;; (defn mf-a "Type signature: a -> m b" [x] (mm-result (f-a x)))
;; (defn mf-b "Type signature: a -> m b" [x] (mm-result (f-b x)))
;; (defn mf-c "Type signature: a -> m b" [x] (mm-result (f-c x)))

(def fn8 (with-monad cont-m (m-chain [mf-a mf-b mf-c])))
((fn8 10) identity) ;; => 21

(defn halt
  "Each time called its return is passed back, up through all the call stack
  layers to the top level function on the call stack.

  Call stack is not cleared. See `bounce`."
  [x]
  (fn [c] (identity x)))

((fn9 10) identity) ;; => 11 (halted)

(defn bounce
  "Each time called (via `trampoline`) its return is passed back, up through all
  the call stack layers to the top level function on the call stack.

  Call stack is cleared. See `halt`."
  [x]
  (fn [c]
    (fn []
      (println "Return to the top level fn of the call stack. Clear the stack.")
      (c x))))

(def fn10
  "fn10 is a continuation (function). It contains `bounce` in its execution
  steps, so is must be invoked using `trampoline`."
  (with-monad cont-m (m-chain [mf-a bounce mf-b bounce mf-c])))
(trampoline ((fn10 10) identity))

(defn mark
  "It causes some delayed calculation and looping... hmm.
  See http://www.clojure.net/2012/03/24/Continuation-monad/"
  [x]
  (fn [c]
    c))

(def fn11 (with-monad cont-m (m-chain [mf-a mark mf-b mf-c])))
(def mark-cont ((fn11 10) identity))
(doall (map mark-cont [0 1 2]))

(defn square [x] (* x x))

(defn f1 [x] [(dec x) x (inc x)]) ;; (f1 5) => [4 5 6]

(defn f2 [x] [x (square x)])      ;; (f2 5) => [5 25]

#_(meditations
   "bla"
   (= [1 2 3 3 4 5]
      ((with-monad sequence-m (m-chain [f2 f1])) 2)))
