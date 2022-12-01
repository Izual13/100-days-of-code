(ns  aoc2021.d1
  (:require [clojure.string :as str]))

(def input-from-file
  (map #(Integer/parseInt %)
    (str/split-lines
      (slurp "resources/aoc2021/day1_1"))))

(defn count-measurement-increases
  ([input] (count-measurement-increases (first input) (rest input) 0))
  ([element input result]
   (if (empty? input)  result
     (let [second (first input)]
       (if (< element second) 
         (count-measurement-increases second (rest input) (inc result))
         (count-measurement-increases second (rest input) result))))))

(count-measurement-increases input-from-file)



(defn three-measurement 
  ([input] (let [e1 (nth input 0) e2 (nth input 1) e3 (nth input 2)] (three-measurement e1 e2 e3 (nthrest input 3) (+ e1 e2 e3) 0)))
  ([e1 e2 e3 input sum result]
   ;(println e1 e2 e3 input sum result)
   (if (empty? input)
     (if (> (+ e1 e2 e3) sum) (inc result) result)
     (if (> (+ e1 e2 e3) sum)
       (three-measurement e2 e3 (first input) (rest input) (+ e1 e2 e3) (inc result))
       (three-measurement e2 e3 (first input) (rest input) (+ e1 e2 e3) result)))))



(three-measurement input-from-file)


(three-measurement [199 200 208 210 200 207 240 269 260 263])

(nthrest input-from-file 3)

(nth input-from-file 2)
