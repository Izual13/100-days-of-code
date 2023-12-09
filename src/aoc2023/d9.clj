(ns  aoc2023.d9
  (:require [clojure.string :as str]))

(def history (clojure.string/split (slurp "resources/aoc2023/day9_1") #"\r?\n"))

(def history-t (clojure.string/split (slurp "resources/aoc2023/day9_t") #"\r?\n"))

(defn parse-array [s] 
  (let [r (clojure.string/split s #" +")]
    (map #(Integer/parseInt %1) r)))

(defn calc [m] 
  (loop [m (vec m) r (last m)]
    (if (= (count (frequencies m)) 1)
      r
      (let [new-m (vec (map-indexed #(- %2 (nth m %1)) (next m)))]
        (recur new-m (+ r (last new-m)))))))

(assert (= 18 (calc [0 3 6 9 12 15])))
(assert (= -3 (calc [15 12 9 6 3 0])))
(assert (= 68 (calc [10  13  16  21  30  45])))

(assert (= 1 (count (frequencies [3 3 3 3 3]))))

(assert (= [15 12 9 6 3 0] (vec (reverse [0 3 6 9 12 15]))))


(assert (= 114 (->> history-t
                 (map (comp calc parse-array))
                 (apply +))))

(assert (= 2 (->> history-t
               (map (comp calc reverse parse-array))
               (apply +))))

(assert (= 1789635132 (->> history
                        (map (comp calc parse-array))
                        (apply +))))

(assert (= 913 (->> history
                 (map (comp calc reverse parse-array))
                 (apply +))))
