(ns  aoc2024.d14
  (:require [clojure.string :as str]
            [clj-async-profiler.core :as prof]))

(def test-robots (str/split (slurp "resources/aoc2024/d14_t") #"\n"))
(def robots (str/split (slurp "resources/aoc2024/d14_1") #"\n"))

(defn parse-robot [s]
  (let [[_ px py vx vy] (re-matches #"p=(-?\d*),(-?\d*) v=(-?\d*),(-?\d*)" s)]
        [(Integer/parseInt px) (Integer/parseInt py) (Integer/parseInt vx) (Integer/parseInt vy)]))


(defn print-robot-map[i max-x max-y rs]
  (let [m (vec (for [i (range max-y)] (vec (for [j (range max-x)] 0))))]
    (loop [rs rs m m]
      (if (empty? rs) 
        (let[tree (-> (str/join "\n" (mapv #(str/join "" %) m))
                          (str/replace  #"0" " ")
                          (str/replace  #"\d" "*"))]
          (if (str/includes? tree "*******************************")
            (println i "\n" (-> (str/join "\n" (mapv #(str/join "" %) m))
                          (str/replace  #"0" " ")
                          (str/replace  #"\d" "*")) "\n\n")
            ))
        (let [f (first rs)
              [x y _ _] f] 
          (recur (next rs) (update-in m [y x] inc)))))))

(defn simulate
  ([max-x max-y rs] (vec (for [r rs
             :let [[x y vx vy] r]] [(mod (+ x vx) max-x) (mod (+ y vy) max-y) vx vy])))
  ([max-x max-y n rs] (loop [i 0 rs rs]
            (print-robot-map i max-x max-y rs)
            (if (= i n) 
              rs
              (recur (inc i) (simulate max-x max-y rs))))))

(defn calculate[max-x max-y rs]
  (let [mx (int (/ max-x 2)) 
        my (int (/ max-y 2))
        max-x (inc max-x)
        max-y (inc max-y)]
    (loop [rs rs q [0 0 0 0]]
    (if (empty? rs) 
      (apply * q)
      (let [[x y] (first rs)]
        (cond
          (and (< -1 x mx) (< -1 y my)) (recur (next rs) (update q 0 inc))
          (and (< mx x max-x) (< -1 y my)) (recur (next rs) (update q 1 inc))
          (and (< -1 x mx) (< my y max-y)) (recur (next rs) (update q 2 inc))
          (and (< mx x max-x) (< my y max-y)) (recur (next rs) (update q 3 inc))
          :else (recur (next rs) q)))))))

(assert (= 12 (->> test-robots
  (mapv parse-robot)
  (simulate 11 7 100)
  (calculate 11 7))))

(assert (= 225521010 (->> robots
  (mapv parse-robot)
  (simulate 101 103 100)
  (calculate 101 103))))
  

(->> robots
  (mapv parse-robot)
  (simulate 101 103 8000))



