(ns  aoc2024.d1
  (:require [clojure.string :as str]))

(def test_lists (clojure.string/split (slurp "resources/aoc2024/d1_t") #"\n"))
(def lists (clojure.string/split (slurp "resources/aoc2024/d1_1") #"\n"))


(defn parse-lists [x]
  (let [c (count x)]
    (loop [i 0 l1 [] l2 []]
      (if (= i c)
        [(vec (sort l1)) (vec (sort l2))]
        (let [f (get x i)
              [_ n1 n2] (re-matches #"(\d+)   (\d+)" f)]
          (recur (inc i) (conj l1 (Integer/parseInt n1)) (conj l2 (Integer/parseInt n2))))
        ))))

(defn calculate [[l1 l2]]
  (let [c (count l1)]
    (loop [i 0 r 0]
      (if (= i c)
        r
        (let [f1 (get l1 i)
              f2 (get l2 i)]
          (recur (inc i) (+ r (abs (- f1 f2)))))
        ))))

(assert (= 11 (->> test_lists
  parse-lists
  calculate)))


(assert (= 2367773 (->> lists
  parse-lists
  calculate)))


(defn parse-lists-2 [x]
  (let [c (count x)]
    (loop [i 0 l1 [] l2 []]
      (if (= i c)
        [(vec l1) (frequencies l2)]
        (let [f (get x i)
              [_ n1 n2] (re-matches #"(\d+)   (\d+)" f)]
          (recur (inc i) (conj l1 (Integer/parseInt n1)) (conj l2 (Integer/parseInt n2))))
        ))))

(defn calculate-2 [[l1 l2]]
  (let [c (count l1)]
    (loop [i 0 r 0]
      (if (= i c)
        r
        (let [f1 (get l1 i)
              f2 (get l2 f1 0)]
          (recur (inc i) (+ r (* f1 f2))))
        ))))


(assert (= 31 (->> test_lists
  parse-lists-2
  calculate-2)))

(assert (= 21271939 (->> lists
  parse-lists-2
  calculate-2)))

