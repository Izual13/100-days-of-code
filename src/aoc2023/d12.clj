(ns  aoc2023.d12
  (:require [clojure.string :as str]))

(def arrangements (clojure.string/split (slurp "resources/aoc2023/day12_1") #"\r?\n"))

(def arrangements-t (clojure.string/split (slurp "resources/aoc2023/day12_t") #"\r?\n"))


(defn parse-arrangement [c] 
  (let [[_ p n] (re-matches #"([?#\.]*) (.*)" c)
        n (clojure.string/split n #",")]
    [(vec p) (mapv (fn [x] (Integer/parseInt x)) n)]))


(parse-arrangement "?###???????? 3,2,1")
(parse-arrangement "???.### 1,1,3")


(defn check-arrangement [p n]
  ; (println p n)
  (loop [p p n n]
    (cond 
      (and (empty? p) (empty? n)) true
      (and (empty? n) (= -1 (.indexOf p \#))) true
      (empty? n) false
      ;(or (empty? p) (empty? n)) false
      (= (first p) \.) (recur (next p) n)
      :else (let [f (take-while #(= \# %1) p)
                  c (count f)]
              (if (= c (first n))
                (recur (nthrest p c) (next n))
                false)))))

(check-arrangement (vec ".#...#....###."))
;[. # . . . # . . . . # # # .]

(defn check-with-permutation [[p n]]
  (loop [p [p] c 0]
    (if (empty? p) 
      c
      (let [f (first p)
            i (.indexOf f \?)]
        (if (= i -1)
          (recur (next p) (if (check-arrangement f n) (inc c) c))
          (recur (conj (next p) (assoc f i \.) (assoc f i \#)) c))))))

(->> arrangements-t
  (map parse-arrangement)
  (map check-with-permutation)
  ; (apply +)
  )

(->> arrangements
  (map parse-arrangement)
  (pmap check-with-permutation)
  (apply +))

(->> arrangements
  (map parse-arrangement)
  (map first)
  (map count)
  frequencies
  ; (map check-with-permutation)
  ; (apply +)
  )


(->> arrangements-t
  (map parse-arrangement)
  (map check-with-permutation)
  (apply +))


(defn parse-arrangement-v2 [c] 
  (let [[_ p n] (re-matches #"([?#\.]*) (.*)" c)
        n (clojure.string/split n #",")]
    [(vec (flatten (repeat 5 (vec p)))) (vec (flatten (repeat 5 (mapv (fn [x] (Integer/parseInt x)) n))))]))


(parse-arrangement "???.### 1,1,3")
(parse-arrangement-v2 "???.### 1,1,3")

(->> arrangements-t
  (map parse-arrangement-v2)
  (pmap check-with-permutation)
  (apply +)
  )

???.###????.###????.###????.###????.### 1,1,3,1,1,3,1,1,3,1,1,3,1,1,3
[[\? \? \? \. \# \# \# \? \? \? \. \# \# \# \? \? \? \. \# \# \# \? \? \? \. \# \# \# \? \? \? \. \# \# \#]
 [1 1 3 1 1 3 1 1 3 1 1 3 1 1 3]]
