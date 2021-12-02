(ns  aoc2021.d1
  (:require [clojure.string :as str]))

(def input-from-file
  (map #(str/split % #" ")
       (str/split-lines
        (slurp "resources/aoc2021/day2_1"))))

(def input-from-file-test
  (map #(str/split % #" ")
       (str/split-lines
        (slurp "resources/aoc2021/day2_t"))))

(defn dive
  ([input] (let [f (first input)] (dive (nth f 0) (Integer/parseInt (nth f 1))  (rest input) 0 0)))
  ([directional number input x y] 
   (if (empty? input) 
     (cond (= directional "forward") (* (+ x number) y)
           (= directional "down") (* x (+ y number))
           (= directional "up") (* x (- y number)))
     (let [next (first input)
           new-d (nth next 0)
           new-n (Integer/parseInt (nth next 1))
           r (rest input)
           _ (println directional number x y)
           ]
       (cond (= directional "forward") (dive new-d new-n r (+ x number) y)
             (= directional "down") (dive new-d new-n r x (+ y number))
             (= directional "up") (dive new-d new-n r x (- y number)))))))


(defn dive2
  ([input] (let [f (first input)] (dive2 (nth f 0) (Integer/parseInt (nth f 1))  (rest input) 0 0 0)))
  ([directional number input x y d]
   (let [new-x (if (= directional "forward") (+ x number) x)
         new-y (cond (= directional "up") (- y number)
                     (= directional "down") (+ y number)
                     :else y)
         new-d (if (= directional "forward") (+ (* y number) d) d)
         _ (comment println directional number new-x new-y new-d)]
     
     (if (empty? input)
       (* new-d new-x)
       (let [next (first input)] (dive2 (nth next 0) (Integer/parseInt (nth next 1)) (rest input) new-x new-y new-d))))))




(dive2 input-from-file)
(dive2 input-from-file-test)

(take-nth 5 input-from-file)

(dive (take-nth 5 input-from-file))

(+ 269 260 263)

(nth (rest []) 0)