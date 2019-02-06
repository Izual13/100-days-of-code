(ns  hdoc.d45
  (:require
   [clojure.test :refer :all]
   [clojure.string :as str]))

;Cavity Map

(defn parse-int [x]
  (loop [result [(mod x 10)]
         reminder (int (Math/floor (/ x 10)))]
    (if (= reminder 0)
      (vec result)
      (recur (cons (mod reminder 10) result) (int (Math/floor (/ reminder 10)))))))


(assert (= [1 1 1 2] (parse-int 1112)))

(defn parse-ints [grid] 
  (vec (for [i grid] 
    (parse-int i))))

(assert (= [[1 1 1 2][1 9 1 2][1 8 9 2][1 2 3 4]] (parse-ints [1112 1912 1892 1234])))

(defn cavityMap [grid]
  (let [grid (parse-ints grid)
        n (count grid)]
    (println 123)
    (vec (for [i (range 0 n)
               j (range 0 n)]
           (if (or (= i 0) (= j 0) (= j (dec n)) (= j (dec n)))
             (get (get grid i) j)
             (let [r1    (get grid (dec i))
                   r2    (get grid i)
                   r3    (get grid (inc i))
                   value (get (get grid i) j)]
               (println r1 r2 r3 value)
               (if (and  (> value (get r1 j))
                         (> value (get r2 (dec j)))
                         (> value (get r2 (inc j)))
                         (> value (get r3 j)))     "X"                    value)        
               ))))))



(get (parse-ints [1112]) 0)

(cavityMap [1112 1912 1892 1234])
