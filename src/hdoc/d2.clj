(ns  hdoc.d2)

;Migratory Birds

(defn migratoryBirds [arr]
  (key (apply max-key val (into (sorted-map-by >) (frequencies arr)))))

(assert (= 3 (migratoryBirds [1 2 3 4 5 4 3 2 1 3 4])))
(assert (= 4 (migratoryBirds [1 4 4 4 5 3])))


;Birthday Chocolate

(defn birthday [s d m] 
  (loop [s s
         c 0
         d d 
         m m
         i 0] 
    (let [li (+ i m)
          cc (if (= d (reduce + (subvec s i li))) (inc c) c)]
      (if (> (inc li) (count s))
        cc
        (recur s cc d m (inc i))))))

(assert (= 2 (birthday [1 2 1 3 2] 3 2)))
(assert (= 0 (birthday [1 1 1 1 1 1] 3 2)))
