(ns  aoc2020.d9
  (:require [clojure.string :as str]))


(def test-numbers (str/split (slurp "resources/aoc2020/day9_t") #"\n"))
(def numbers (str/split (slurp "resources/aoc2020/day9_1") #"\n"))

(defn check [preamble number] 
  (let [s (set preamble)]
    (loop [ts (vec preamble)]
      
           (println ts)
      (cond 
        (empty? ts) false
        (contains? s (- number (first ts))) true
        :else (recur (next ts))))))

(defn check-numbers [p n] 
  (loop [s (take p n) n (drop p n)]
    (if (empty? n) 
      -1
      (let [f (first n)]
        (if (check (vec s) f) 
          (recur (conj (vec (drop 1 s)) (first n)) (drop 1 n))
          f)))))


(assert (= 127 (->> test-numbers
  (map Long/parseLong)
  (check-numbers 5)
  )))

(assert (= 258585477 (->> numbers
  (map Long/parseLong)
  (check-numbers 25))))


(defn check-numbers-m [p n] 
  (loop [s (take p n) n (drop p n) r []]
    (if (empty? n) 
      r
      (let [f (first n)]
        (if (check (vec s) f) 
          (recur (conj (vec (drop 1 s)) (first n)) (drop 1 n) r)
          (recur (conj (vec (drop 1 s)) (first n)) (drop 1 n) (conj r f)))))))

(->> test-numbers
  (map Long/parseLong)
  (check-numbers-m 5))


(->> numbers
  (map Long/parseLong)
  (check-numbers-m 25))

(defn find-encryption-weakness[m]
  (let [c (count m)]
    (for [i (range c)]
      (for [j (range  (inc i) c)]
        (let [n (mapv #(get m %) (range i j))]
          ; n)))))
          
          (if (= 258585477 (apply + n))
            n
            nil))))))

(defn calculate [m]
  (+ (apply min m) (apply max m)))

(->> numbers
  (mapv Long/parseLong)
  find-encryption-weakness
  (mapv #(filterv some? %))
  (filterv #(not (empty? %))))


(assert (= 36981213 (calculate [13858643 9455395 9908827 16794010 13221299 11563238 12646458 11137204 11774548 12220424 14302571 14304519 14748447 25865809 22680253 16578014 27525818])))

