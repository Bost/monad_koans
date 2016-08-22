(defn square [x] (* x x))

(defn f1 [x] [(dec x) x (inc x)]) ;; (f1 5) => [4 5 6]

(defn f2 [x] [x (square x)])      ;; (f2 5) => [5 25]

(defn as-set [x] (fn [y] (apply hash-set (x y))))

(def f1-set (as-set f1))

(def f2-set (as-set f2))

(defn monad-bind [monad-value monad-function] (mapcat monad-function monad-value))

(defn monad-result [x] [x])

(defn monad-compose [fn1 fn2] (fn [x] (-> x monad-result (monad-bind fn2) (monad-bind fn1))))

(meditations
 "Contemplate how the function signatures corelate to the m(onad)-bind and m(onad)-result"
 (= [4 5 6 24 25 26]
    ((fn [x] (->> x f2 (mapcat f1))) 5)
    ((monad-compose f1 f2) 5))

 "Do you think it is comp what you are doing? hm..."
 (= [9 10 11 99 100 101]
    ((fn [x] (domonad sequence-m [a (f2 x) ;; => [10 100]
                                 b (f1 a)]
                      b)) 10))

 " composition is easy"
 (= [1 2 3 3 4 5]
    ((with-monad sequence-m (m-chain [f2 f1])) 2))

 "Nil safety is easy"
 (= nil
    ((with-monad maybe-m (m-chain [f2 f1])) nil))

 "Different data types imply different container monads"
 (= (hash-set 1 2 3 4 5)
    ((with-monad set-m (m-chain [f2-set f1-set])) 2)))
