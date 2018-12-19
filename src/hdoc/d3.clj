(ns  hdoc.d3)

;Divisible Sum Pairs

(defn divisibleSumPairs [n k ar]
   (loop [head (first ar) 
          tail (rest ar)
          c 0] 
          (if (empty? tail) 
          c
          (recur (first tail) (rest tail) 
          (+ c (count (for [x tail
            :let [y (+ x head)]
            :when (= 0 (mod y k))] 
            1)))))))

(assert (= 5 (divisibleSumPairs 6 3 [1 3 2 6 1 2])))

;Day of the Programmer

(defn dayOfProgrammer [year]
  (str (cond
         (and (< year 1918) (= (mod year 4) 0) ) "12"
         (= year 1918) "26" ; 13 + 13 
         (and (> year 1918) (or (= (mod year 400) 0) (and (= (mod year 4) 0) (not (= (mod year 100) 0))))) "12"
         :else "13") ".09." year))


(assert (= "13.09.2017" (dayOfProgrammer 2017)))
(assert (= "26.09.1918" (dayOfProgrammer 1918)))
