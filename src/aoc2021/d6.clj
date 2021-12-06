(ns  aoc2021.d6
  (:require [clojure.string :as str]))

(def input-from-file-test
  (->> (slurp "resources/aoc2021/day6_t")
       (str/split-lines)
       (mapv #(str/split % #","))
       (flatten)
       (mapv #(Integer/parseInt %))))


(def input-from-file
  (->> (slurp "resources/aoc2021/day6_1")
       (str/split-lines)
       (mapv #(str/split % #","))
       (flatten)
       (mapv #(Integer/parseInt %))))



(assert (= [3 4 3 1 2] input-from-file-test))


(defn part1
  ([input] (loop [i 0 input input] 
             (if (= i 80) 
               (count input)
               (let [grouped (group-by #(= 0 %) input)
                     count-0 (count (get grouped true))
                     result (map dec (get grouped false))
                     result (apply conj result (concat (take count-0 (repeat 8)) (take count-0 (repeat 6))))
                     ] 
                 (recur (inc i) result))
               ))))

(assert (= 5934 (part1 input-from-file-test)))
(assert (= 395627 (part1 input-from-file)))

(defn part2
  ([input] (loop [i 0 input (frequencies input)] 
             (if (= i 256) 
               (apply + (map (fn [[_ n]] n) input))
               (let [count-0 (get input 0)
                     count-0 (if (nil? count-0) 0 count-0)
                     result (assoc input 0 0)
                     result (loop [j 1 result result]
                              (if (= j 9)
                                result
                                (let [e (get result j)]
                                  (if (nil? e)
                                    (recur (inc j) (assoc result (dec j) 0))
                                    (recur (inc j) (assoc result (dec j) e))))))

                     count-6 (get result 6)
                     count-6 (if (nil? count-6) 0 count-6) 
                     result (assoc result 6 (+ count-0 count-6))
                     result (assoc result 8 count-0)
                     ] 
                 (recur (inc i) result))
               ))))


(assert (= 26984457539 (part2 input-from-file-test)))
(assert (= 1767323539209 (part2 input-from-file)))
(assert (= {0 30, 2 20} (assoc (array-map 0 10 2 20) 0 30)))


