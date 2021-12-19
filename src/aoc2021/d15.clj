(ns  aoc2021.d15
  (:require [clojure.string :as str]))

(def MAX_VALUE 1000000)

(def input-from-file-test
  (->> (slurp "resources/aoc2021/day15_t")
       (str/split-lines)
       (mapv #(str/split % #""))
       (mapv #(mapv (fn [x] (Integer/parseInt x)) %))))

input-from-file-test

(def input-from-file
  (->> (slurp "resources/aoc2021/day15_1")
       (str/split-lines)
       (mapv #(str/split % #""))
       (mapv #(mapv (fn [x] (Integer/parseInt x)) %))))


(assert (not= [1 2] [2 1]))

(defn build-empty-map [y x] (vec (for [_ (range y)] (vec (for [_ (range x)] MAX_VALUE)))))

(assert (= [[MAX_VALUE] [MAX_VALUE] [MAX_VALUE]] (build-empty-map 3 1)))

(defn get-neighbours [m [y x]]
  (->> [[(dec y) x] [(inc y) x]
        [y (dec x)] [y (inc x)]]
       (filterv #(pos? (get-in m % -1)))))

(assert (= [[0 1] [2 1] [1 0] [1 2]] (get-neighbours [[1 2 3] [4 5 6] [7 8 9]] [1 1])))

(defn update-risks [input risks cursor neighbours]
  (let [c (get-in risks cursor)]
    (loop [n neighbours r risks]
      (if (empty? n)
        r
        (recur (rest n) (update-in r (first n) (fn [x] (min x (+ (get-in input (first n)) c)))))))))

(defn part1 [input]
  (let [y-count (count input)
        x-count (count (first input))
        maxx (dec x-count)
        maxy (dec y-count)
        target [maxy maxx]
        counts (-> (build-empty-map y-count x-count)
                   (assoc-in [0 0] 0))
        result (loop [cursor [0 0] heap #{[0 0]} visited #{} result counts]
                 (if (or (= cursor target) (empty? heap))
                   (get-in result target)
                   (let [neighbours (->> (get-neighbours result cursor)
                                         (filter #(not (contains? visited %))))
                         result' (update-risks input result cursor neighbours)
                         heap' (apply conj heap neighbours)
                         heap' (disj heap' cursor)
                         cursor' (first (sort-by #(get-in result' %) heap'))]
                     (recur cursor' heap' (conj visited cursor) result'))))] result))




(assert (= 40 (part1 input-from-file-test)))
(assert (= 487 (time (part1 input-from-file))))

(defn part2 [input]
  (let [] 0))

(assert (= 2188189693529 (time (part2 input-from-file-test))))
(assert (= 2566282754493 (time (part2 input-from-file))))

