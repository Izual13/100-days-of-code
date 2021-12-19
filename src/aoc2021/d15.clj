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
        (let [f (first n)
              old-value (get-in r f)
              new-value (+ (get-in input f) c)]
          (if (< new-value old-value)
            (recur (rest n) (assoc-in r f new-value))
            (recur (rest n) r)))))))

(defn min-by [fn coll]
  (->> (reduce #(let [n (fn %2)]
                  (cond (nil? %1) [n %2]
                        (> (first %1) n) [n %2]
                        :else %1)) nil coll)
       (second)))


(assert (= 1 (min-by #(inc %) #{5 1 2 3 4})))

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
                         cursor' (min-by #(get-in result' %) heap')]
                     (recur cursor' heap' (conj visited cursor) result'))))] result))


(assert (= 40 (part1 input-from-file-test)))
(assert (= 487 (time (part1 input-from-file))))

(defn input-x5 [input]
  (let [y-count (count input)
        x-count (count (first input))
        next-value (fn [v y x] (let [m (+ (int (/ y y-count)) (int (/ x x-count)))
                                     r (+ v m)]
                                 (if (< r 10) r (mod r 9))))
        ]
    (vec (for [y (range (* 5 y-count))]
           (vec (for [x (range (* 5 x-count))] (next-value (get-in input [(mod y y-count) (mod x x-count)]) y x)))))))


(defn part2 [original-input]
  (let [input (input-x5 original-input)
        y-count (count input)
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
                         cursor' (min-by #(get-in result' %) heap')]
                     (recur cursor' heap' (conj visited cursor) result'))))] result))

(assert (= 315 (time (part2 input-from-file-test))))
(assert (= 2821 (time (part2 input-from-file))))

