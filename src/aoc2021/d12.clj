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
            f (frequencies path)
            isSingle (empty? (filter (fn [[p c]] (and (Character/isLowerCase (first p)) (= c 2))) f))
            visited (set (filter (fn [p] (or (= p "start") (and (Character/isLowerCase (first p)) (not isSingle)))) path))
            result (filter #(not (contains? visited %)) result)] result))))


(assert (false? (empty? (filter (fn [[p c]] (and (Character/isLowerCase (first p)) (= c 2))) (frequencies ["start","A","b","A","b","A","c","A","end"])))))
(assert (true? (empty? (filter (fn [[p c]] (and (Character/isLowerCase (first p)) (= c 2))) (frequencies ["start","A","b","A","A","c","A","end"])))))

(defn get-new-paths [path nodes]
  (for [n nodes] (conj path n)))

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
    (count (filter #(= "end" (last %)) paths))))

(assert (= 10 (part1 input-from-file-test)))
(assert (= 4970 (part1 input-from-file)))


(defn part2 [input]
  (let [graph (build-graph input)]
    (loop [v #{["start"]} r 0]
      (if (empty? v)
        r
        (let [l (first v)
              nodes (get-nodes2 graph l)
              ]
          (if (empty? nodes)
            (if (= "end" (last l))
              (recur (disj v l) (inc r))
              (recur (disj v l) r))
            (recur (apply conj (disj v l) (get-new-paths l nodes)) r)))))))

(assert (= 36 (time (part2 input-from-file-test))))
(assert (= 137948 (time (part2 input-from-file))))


