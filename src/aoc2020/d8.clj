(ns  aoc2020.d8
  (:require [clojure.string :as str]))


(def test-program (str/split (slurp "resources/aoc2020/day8_t") #"\n"))
(def program (str/split (slurp "resources/aoc2020/day8_1") #"\n"))


(defn parse-program [p] 
  (let [[_ k v] (re-matches #"(.*) (.*)" p)]
    [k (Integer/parseInt v)]))

(defn execute [p] 
  (loop [i 0 r 0 v #{}]
    (cond 
      (= i (count p)) r
      (contains? v i) r
      :else (let [[k n] (get p i)]
              (cond 
                (= k "nop") (recur (inc i) r (conj v i))
                (= k "acc") (recur (inc i) (+ r n) (conj v i))
                (= k "jmp") (recur (+ i n) r (conj v i))
                :else v)))))

(->> test-program
  (mapv parse-program)
  execute)


(->> program
  (mapv parse-program)
  execute)


(defn fixed-execute [fixed p] 
  (loop [i 0 r 0 v #{}]
    (cond 
      (= i (count p)) r
      (contains? v i) nil
      :else (let [[k n] (get p i)
                  attempt (if (and (nil? fixed) (or (= k "nop") (= k "jmp"))) (fixed-execute i p) nil)]
              (cond 
                (some? attempt) attempt
                (and (= k "nop") (= i fixed)) (recur (+ i n) r (conj v i))
                (and (= k "jmp") (= i fixed)) (recur (inc i) r (conj v i))
                (= k "nop") (recur (inc i) r (conj v i))
                (= k "acc") (recur (inc i) (+ r n) (conj v i))
                (= k "jmp") (recur (+ i n) r (conj v i))
                :else v)))))

(assert (= 8 (->> test-program
  (mapv parse-program)
  (fixed-execute nil))))

(assert (= 1174 (->> program
  (mapv parse-program)
  (fixed-execute nil))))