(ns  aoc2024.d2
  (:require [clojure.string :as str]))

(def test_numbers (clojure.string/split (slurp "resources/aoc2024/d2_t") #"\n"))
(def numbers (clojure.string/split (slurp "resources/aoc2024/d2_1") #"\n"))


lists

(defn parse-numbers [x]
  (mapv #(Integer/parseInt %) (str/split x #" ")))


(defn check [numbers]
  (let [p1 (loop [i 0 n numbers]
             (cond 
               (= (dec (count n)) i) true
               (and (> (get n i) (get n (inc i))) (< 0 (- (get n i) (get n (inc i))) 4)) (recur (inc i) n)
               :else false))
        p2 (loop [i 0 n numbers]
             (cond 
               (= (dec (count n)) i) true
               (and (> (get n (inc i)) (get n i)) (< 0 (- (get n (inc i)) (get n i)) 4)) (recur (inc i) n)
               :else false))]
    (or p1 p2)))

(assert (= 2 (->> 
  test_numbers
  (mapv parse-numbers)
  (mapv check)
  (filter identity)
  count)))

(assert (= 224 (->> 
  numbers
  (mapv parse-numbers)
  (mapv check)
  (filter identity)
  count)))

(defn vec-remove
  "remove elem in coll"
  [pos coll]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn check-2 [numbers]
  (let [p1 (loop [i 0 n numbers b false]
             (cond 
               (= (dec (count n)) i) true
               (and (> (get n i) (get n (inc i))) (< 0 (- (get n i) (get n (inc i))) 4)) (recur (inc i) n b)
               (and (not b) (not= (- (count n) 2) i) (> (get n i) (get n (+ i 2))) (< 0 (- (get n i) (get n (+ i 2))) 4)) (recur 0 (vec-remove (inc i) n) true)
               (and (not b) (= (- (count n) 2) i)) true
               (not b) (recur 0 (vec-remove i n) true)
               :else false))
        p2 (loop [i 0 n numbers b false]
             (cond 
               (= (dec (count n)) i) true
               (and (> (get n (inc i)) (get n i)) (< 0 (- (get n (inc i)) (get n i)) 4)) (recur (inc i) n b)
               (and (not b) (not= (- (count n) 2) i) (> (get n (+ i 2)) (get n i)) (< 0 (- (get n (+ i 2)) (get n i)) 4)) (recur 0 (vec-remove (inc i) n) true)
               (and (not b) (= (- (count n) 2) i)) true
               (not b) (recur 0 (vec-remove i n) true)
               :else false))]
    (or p1 p2)))

(assert (= 4 (->> 
  test_numbers
  (mapv parse-numbers)
  (mapv check-2)
  (filter identity)
  count)))

(assert (= 293 (->> 
  numbers
  (mapv parse-numbers)
  (mapv check-2)
  (filter identity)
  count)))