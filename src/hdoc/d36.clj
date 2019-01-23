(ns  hdoc.d36
  (:require
   [clojure.string :as str]))

;2D Array - DS

(defn get-glass [arr i j] 
  (let  [row1 (get arr i)
         row2 (get arr (+ i 1))
         row3 (get arr (+ i 2))]
    (+ (get row1 j) (get row1 (+ j 1)) (get row1 (+ j 2)) 
     (get row2 (+ j 1))
     (get row3 j) (get row3 (+ j 1)) (get row3 (+ j 2)))))

(defn hourglassSum [arr]
  (apply max (let [size (- (count arr) 2)
        s (for [i (range size)
                j (range size)]
              (get-glass arr i j))] s)))


(assert (= 19 (hourglassSum [[1 1 1 0 0 0] [0 1 0 0 0 0] [1 1 1 0 0 0] [0 0 2 4 4 0] [0 0 0 2 0 0] [0 0 1 2 4 0]])))
