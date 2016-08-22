(defn increment
  "Returns monadic values with structure [result new-state]"
  [state-int]
  ["incremented!" (inc state-int)])

(defn decrement
  "Returns monadic values with structure [result new-state]"
  [state-int]
  ["decremented!" (dec state-int)])

(defn next-rand-int [from]
  "Generates random int between 'from' inclusive and Integer$MAX_VALUE exclusive.
   Returns the random and the next int which can be feed into the next call as from."
  (let [rand-between (fn [f t] (+ (rand-int (- t f)) f))
        random (rand-between from
                             Integer/MAX_VALUE)]
    ;; [result new-state]
    [random (inc random)]))

(meditations
 "Dynamic balance means lots of movement but no change
Realise that the increment and decrement functions are monadic values for the state monad"
 (= '(["incremented!" "decremented!"] 5)
    ((domonad state-m [a increment
                       b decrement]
              [a b]) 5))

 "You are allowed to peek"
 (= '(["incremented!" 6 "decremented!"] 5)
    ((domonad state-m [a increment
                       x (fetch-state)
                       b decrement]
              [a x b]) 5))

 "Why not take controll even?"
 (= '(["incremented!" 6 "decremented!"] 42)
    ((domonad state-m [a increment      ;; => {:result "incremented!" :new-val 6}
                       x (set-state 43) ;; => {:result 6 :new-val 43}
                       b decrement]     ;; => {:result "decremented!" :new-val 42}
              [a x b]) 5))

 "Let's generate a strictly monotonically increasing sequence with three elements
  Contemplate how the plumbing between the functions is hidden"
 (let [[inc-seq state] ((domonad state-m [rnd1 next-rand-int
                                          rnd2 next-rand-int
                                          rnd3 next-rand-int]
                                 (list rnd1 rnd2 rnd3)) 10)]
   (and (< (first inc-seq) (second inc-seq))
        (< (second inc-seq) (last inc-seq))
        (< (last inc-seq) state))))
