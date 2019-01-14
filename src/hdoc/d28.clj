(ns  hdoc.d28
  (:require
   [clojure.string :as str]))

;ACM ICPC Team


(defn calc [a1 a2]
  (let [length (count a1)]
    (reduce + (for [i (range length)] (if (or (= (get a1 i) \1) (= \1 (get a2 i))) 1 0)))))

(defn maxCount [s]
  (loop [max 0
         count 0
         s (seq s)]
    (if (empty? s) 
      [max count]
      (let [f     (first s)
            count (cond 
                    (= max f) (inc count) 
                    (> f max) 1
                    :else  count)
            max   (if (> f max) f max)]
        (recur max count (next s))))))


(get (boolean-array (map #(if (= % \1) true false) "1011")) 0)


(defn acmTeam [topic]
  (maxCount (let [n (count topic)]
    (for [x     (range n)
          y     (range n)
          :when (and (not= x y) (< x y))] 
      (calc (nth topic x) (nth topic y))
      ))))

(count [1 2 3])

(acmTeam ["11101" "10101" "11001" "10111" "10000" "01110"])


(assert (= [5 6] (acmTeam ["11101" "10101" "11001" "10111" "10000" "01110"])))
(assert (= [5 2] (acmTeam ["10101" "11100" "11010" "00101"])))
