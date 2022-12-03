(ns aoc2022.d3
  (:require [clojure.string :as str]))

(def alphabet "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn parse-rucksack [r] (let [c (count r) h (/ c 2)] [(set (subs r 0 h)) (set (subs r h))]))

(def test-rucksacks (clojure.string/split (slurp "resources/aoc2022/day3_t") #"\n"))
(def rucksacks (clojure.string/split (slurp "resources/aoc2022/day3_1") #"\n"))

(defn calc-weight [x] (+ 1 (str/index-of alphabet x)))

(assert (= 2 (calc-weight \b)))

(defn find-letter [[f l]] 
  (loop [l l] (if (empty? l) 
                nil
                (if (contains? f (first l)) 
                  (first l)
                  (recur (rest l))))))

(assert (= \p (find-letter [(set "vJrwpWtwJgWr") (set "hcsFMMfFFhFp")])))

(assert (= 157 (->> test-rucksacks
                 (map parse-rucksack)
                 (map find-letter)
                 (map calc-weight)
                 (apply +))))

(assert (= 8394 (->> rucksacks
                  (map parse-rucksack)
                  (map find-letter)
                  (map calc-weight)
                  (apply +))))

(assert (= 70 (->>   
                (loop [r test-rucksacks result []]  
                  (if (empty? r) 
                    result
                    (let[_ (println (map set (take 3 r)))
                         s (apply clojure.set/intersection (map set (take 3 r)))
                         _ (println s)
                         ]
                      (recur (nthrest r 3) (apply conj result s)))))
                (map calc-weight)
                (apply +)
                )))

(assert (= 2413 (->>   
                  (loop [r rucksacks result []]  
                    (if (empty? r) 
                      result
                      (let[_ (println (map set (take 3 r)))
                           s (apply clojure.set/intersection (map set (take 3 r)))
                           _ (println s)
                           ]
                        (recur (nthrest r 3) (apply conj result s)))))
                  (map calc-weight)
                  (apply +)
                  )))
  
  
(assert (= #{:c :b} (apply clojure.set/intersection [#{:a :b :c} #{:d :c :b}])))

(assert (= [4 5 6] (nthrest [1 2 3 4 5 6] 3)))