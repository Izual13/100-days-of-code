(ns  aoc2023.d9
  (:require [clojure.string :as str]))

(def history (clojure.string/split (slurp "resources/aoc2023/day9_1") #"\r?\n"))

(def history-t (clojure.string/split (slurp "resources/aoc2023/day9_t") #"\r?\n"))

(defn parse-array [s] 
  (let [r (clojure.string/split s #" +")]
    (mapv #(Integer/parseInt %1) r)))


(defn calc [m] 
  (loop [m m r (last m)]
    (if (= (count(frequencies m)) 1)
      r
      (let [new-m (map-indexed (fn [idx itm] 
                                 (- itm (get m idx))) (next m))]
        (recur (vec new-m) (+ r (last new-m)))))))


(assert (= 18 (calc [0 3 6 9 12 15])))
(assert (= -3 (calc [15 12 9 6 3 0])))
(assert (= 68 (calc [10  13  16  21  30  45])))

(assert (= 1 (count (frequencies [3 3 3 3 3]))))

(assert (= [15 12 9 6 3 0] (vec (reverse [0 3 6 9 12 15]))))


(assert (= 114 (->> history-t
                 (map parse-array)
                 (map calc)
                 (apply +))))

(assert (= 2 (->> history-t
               (map (comp calc vec reverse parse-array))
               (apply +))))


(assert (= 1789635132 (->> history
                        (map parse-array)
                        (map calc)
                        (apply +))))

(assert (= 913 (->> history
                 (map (comp calc vec reverse parse-array))
                 (apply +))))
