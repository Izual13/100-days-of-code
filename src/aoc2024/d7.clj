(ns  aoc2024.d7
  (:require [clojure.string :as str]))


(def test-equations (str/split (slurp "resources/aoc2024/d7_t") #"\n"))
(def equations (str/split (slurp "resources/aoc2024/d7_1") #"\n"))


(defn parse-equation [e]
  (let [[_ n1 n2] (re-matches #"(\d+): (.*)" e)] [(Long/parseLong n1) (mapv Long/parseLong (str/split n2 #" "))]))

(defn try-calibrate
  ([[r n]] (try-calibrate r (next n) (first n))) 
  ([r n t] (cond 
             (> t r) nil
             (nil? r) nil
             (and (= t r) (empty? n)) r
             (empty? n) nil
             :else (let [f1 (try-calibrate r (next n) (* t (first n)))
                         f2 (try-calibrate r (next n) (+ t (first n)))
                         ]
                     (if (and (nil? f1) (nil? f2)) 
                       nil
                       r)))))

(defn try-calibrate-2 
  ([[r n]] (try-calibrate r (next n) (first n))) 
  ([r n t] (cond 
             (> t r) nil
             (nil? r) nil
             (and (= t r) (empty? n)) r
             (empty? n) nil
             :else (let [f1 (try-calibrate r (next n) (* t (first n)))
                         f2 (try-calibrate r (next n) (+ t (first n)))
                         f3 (try-calibrate r (next n) (Long/parseLong (str t (first n))))
                         ; _ (println f1)
                         ; _ (println f2)
                         ; _ (println f3)
                         ]
                     (if (and (nil? f1) (nil? f2) (nil? f3)) 
                       nil
                       r)))))

(assert (= 3749 (->> test-equations
  (mapv parse-equation)
  (mapv try-calibrate)
  (filter identity)
  (apply +))))

(assert (= 6231007345478 (->>  equations
  (mapv parse-equation)
  (mapv try-calibrate)
  (filter identity)
  (apply +))))


(assert (= 11387 (->> test-equations
  (mapv parse-equation)
  (mapv try-calibrate-2)
  (filter identity)
  (apply +))))

(assert (= 333027885676693 (->>  equations
  (mapv parse-equation)
  (mapv try-calibrate-2)
  (filter identity)
  (apply +))))
