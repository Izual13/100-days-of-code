(ns  aoc2024.d19
  (:require [clojure.string :as str]
            [clj-async-profiler.core :as prof])
   (:use     [utils]))


(def test-designs (str/split (slurp "resources/aoc2024/d19_t") #"\n\n"))
(def designs (str/split (slurp "resources/aoc2024/d19") #"\n\n"))


(defn parse-designs [[patterns designs]]
  [(str/split patterns #", ") (str/split designs #"\n")])

(defn check-design [patterns design cache]
  (cond 
    (contains? cache design) [(cache design) cache]
    (= ""  design) [true (assoc cache "" true)]
    :else (loop [p patterns cache cache]
            (cond 
              (empty? p) [false (assoc cache design false)]
              (str/starts-with? design (first p)) (let [[isPossible cache'] (check-design patterns (subs design (count (first p))) cache)]
                                                    (if isPossible 
                                                      [isPossible cache']
                                                      (recur (next p) cache')))
              :else (recur (next p) cache)))))

(defn check-designs [[patterns designs]]
  (loop [d designs cache {} r 0] 
    (if (empty? d) 
      r
      (let [[isPossible cache'] (check-design patterns (first d) cache)]
        (recur (next d) cache' (if isPossible (inc r) r))))))

(assert (= 6 (->> test-designs
  parse-designs
  check-designs)))

(assert (= 355 (->> designs
  parse-designs
  check-designs)))


(defn check-design2 [patterns design cache]
  (cond 
    (contains? cache design) [(cache design) cache]
    (= ""  design) [1 (assoc cache "" 1)]
    :else (loop [p patterns cache cache r 0]
            (cond 
              (empty? p) [r (assoc cache design r)]
              (= design (first p)) (recur (next p) (assoc cache design 1) (inc r))
              (str/starts-with? design (first p)) (let [[r' cache'] (check-design2 patterns (subs design (count (first p))) cache)]
                                                    (recur (next p) cache' (+ r' r)))
              :else (recur (next p) cache r)))))


(defn check-designs2 [[patterns designs]]
  (loop [d designs cache {} r 0] 
    (if (empty? d) 
      r
      (let [[result cache'] (check-design2 patterns (first d) cache)]
        (recur (next d) cache' (+ result r))))))

(assert (= 16 (->> test-designs
  parse-designs
  check-designs2)))

(assert (= 732978410442050 (->> designs
  parse-designs
  check-designs2)))