(ns  aoc2020.d2
  (:require [clojure.string :as str]))

(defn parse-password [s]
  (let [[_ a b c d] (re-matches #"(\d*)-(\d*) (.): (.*)" s)]
    {:indicates [(Integer/parseInt a) (Integer/parseInt b)] :l (get c 0) :p d}))

(def passwords (clojure.string/split (slurp "resources/aoc2020/day2_1") #"\n"))

(defn check-password [p] 
  (let[f (frequencies (:p p))
       [a b] (:indicates p)
       l (f (:l p))]
    (cond 
      (nil? l) false
      (<= a l b) true
      :else false)))

(assert (= true (check-password {:indicates [4 5], :l \l, :p "rllllj"})))

(assert (= 434 (->> passwords
                 (map parse-password)
                 (map check-password)
                 (filter #(= true %))
                 count)))


(defn check-password2 [p] 
  (let[[a b] (:indicates p)
       l (:l p)
       p (:p p)]
    (println p a b l)
    (cond 
      (and (= l (get p (dec a))) (nil? (get p (dec b)))) true
      (and (= l (get p (dec a))) (not= l (get p (dec b)))) true
      (and (= l (get p (dec b))) (not= l (get p (dec a)))) true
      :else false)))


(assert (= true (check-password2 {:indicates [1 3], :l \a, :p "abcde"})))
(assert (= false (check-password2 {:indicates [1 9], :l \c, :p "ccccccccc"})))
(assert (= false (check-password2 {:indicates [1 3], :l \b, :p "cdefg"})))


(assert (= 509 (->> passwords
                 (map parse-password)
                 (map check-password2)
                 (filter #(= true %))
                 count)))

