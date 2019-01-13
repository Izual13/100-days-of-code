(ns  hdoc.d28
  (:require
   [clojure.string :as str]))

;ACM ICPC Team

(defn permutation [n]
  (for [x (range n) y (range n) :when (and (not= x y) (< x y))] [x y]))

(defn calc [a1 a2]
  (println a1)
  (println a2)
  (let [length (count a1)]
    (loop [i 0
           result 0]
      (println "i: " i " = " result)
      (if (= i length)
        result
        (recur (inc i) (+ result (if (or (= (get a1 i) \1) (= \1 (get a2 i))) 1 0)))))))

(defn acmTeam [topic]
  (let [n (count topic) 
        p (permutation n)
        out (println p)
        g (group-by (fn [x] 
                      (println "x: " x)
                      (calc (nth topic (first x)) (nth topic (last x))))
                    p)
        max (apply max-key key g)]
    [(key max) (count (val max))])
  )


(time
 (calc "1001101111101011011100101100100110111011111011000100111100111110111101011011011100111001100011111010" "1111010101101010011101101101011101111111111011110010001001100111000111011111101110010111110111110010"))




(assert (= [5 6] (acmTeam ["11101" "10101" "11001" "10111" "10000" "01110"])))
(assert (= [5 2] (acmTeam ["10101" "11100" "11010" "00101"])))
