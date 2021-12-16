(ns  aoc2021.d12
  (:require [clojure.string :as str]))


(def input-from-file-test
  (->> (slurp "resources/aoc2021/day12_t")
       (str/split-lines)
       (mapv #(str/split % #"-"))))



(def input-from-file
  (->> (slurp "resources/aoc2021/day12_1")
       (str/split-lines)
       (mapv #(str/split % #"-"))))


(defn build-graph [t]
  (loop [map {} v t]
    (if (empty? v)
      map
      (let [[s e] (first v)
            new-map (update-in map [s] (fnil #(conj % e) #{}))
            new-map (update-in new-map [e] (fnil #(conj % s) #{}))]
        (recur new-map (rest v))))))

(assert (= {"start" #{"b" "A"}, "A" #{"start" "b" "end" "c"}, "b" #{"d" "start" "A" "end"}, "c" #{"A"}, "d" #{"b"}, "end" #{"b" "A"}} (build-graph input-from-file-test)))

(defn get-nodes [graph path]
  (let [l (last path)]
    ;(println "l: " l)
    (if (= l "end")
      []
      (let [result (get graph l)
            visited (set (filter #(Character/isLowerCase (first %)) path))
            result (filter #(not (contains? visited %)) result)] result))))


(defn get-nodes2 [graph path]
  (let [l (last path)]
    ;(println "l: " l)
    (if (= l "end")
      []
      (let [result (get graph l)
            visited (set (map first (filter (fn [[p c]] (or (= p "start") (= p "end") (and (Character/isLowerCase (first p)) (= c 2)))) (frequencies path))))
            result (filter #(not (contains? visited %)) result)] result))))


(defn get-new-paths [path nodes]
  (vec (for [n nodes] (conj path n))))

(assert (= [["aa" 1] ["aa" 2] ["aa" 3]] (get-new-paths ["aa"] [1 2 3])))

(defn part1 [input]
  (let [graph (build-graph input)
        paths (loop [v #{["start"]} r #{}]
                (if (empty? v)
                  r
                  (let [l (first v)
                        nodes (get-nodes graph l)]
                    (if (empty? nodes)
                      (recur (set (rest v)) (set (conj r l)))
                      (recur (set (apply conj (rest v) (get-new-paths l nodes))) r)))))]
    (println (filter #(= "end" (last %)) paths))
    (count (filter #(= "end" (last %)) paths))))

(assert (= 10 (part1 input-from-file-test)))
(assert (= 4970 (part1 input-from-file)))


(defn part2 [input]
  (let [graph (build-graph input)
        paths (loop [v #{["start"]} r #{}]
                (if (empty? v)
                  r
                  (let [l (first v)
                        nodes (get-nodes2 graph l)]
                    (if (empty? nodes)
                      (recur (set (rest v)) (set (conj r l)))
                      (recur (set (apply conj (rest v) (get-new-paths l nodes))) r)))))]
    (println (filter #(= "end" (last %)) paths))
    (count (filter #(= "end" (last %)) paths))))

(assert (= 195 (part2 input-from-file-test)))
(assert (= 371 (part2 input-from-file)))


