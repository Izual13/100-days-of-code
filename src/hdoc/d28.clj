(ns  hdoc.d28
  (:require
   [clojure.string :as str]))

;ACM ICPC Team

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


(defn acmTeam [topic]
  (maxCount (let [n (count topic)
                  topic (vec (map #(BigInteger. % 2)  topic))]
              (for [x     (range n)
                    y     (range n)
                    :when (and (not= x y) (< x y))]
                (let [intersection (.or (get topic x) (get topic y))]
                  (.bitCount intersection))))))

(acmTeam ["11101" "10101" "11001" "10111" "10000" "01110"])

(assert (= [5 6] (acmTeam ["11101" "10101" "11001" "10111" "10000" "01110"])))
(assert (= [5 2] (acmTeam ["10101" "11100" "11010" "00101"])))

