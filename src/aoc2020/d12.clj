(ns  aoc2020.d12
  (:require [clojure.string :as str]))


(def test-instructions (str/split (slurp "resources/aoc2020/day12_t") #"\n"))
(def instructions (str/split (slurp "resources/aoc2020/day12_1") #"\n"))

(defn parse-instruction [i]
  (let [[_ k v] (re-matches #"(.)(\d+)" i)]
    [(get k 0) (Long/parseLong v)]))

(def direction-map {0 \E 1 \S 2 \W 3 \N})

(direction-map (+ 0 (/ 270 90)))

(defn execute [instructions]
  (loop [i instructions f 0 x 0 y 0]
    (if (empty? i)
      (+ (abs x) (abs y))
      (let [[k v] (first i)
            n (next i)]
        (cond 
          (= k \N) (recur n f x (+ y v))
          (= k \E) (recur n f (+ x v) y)
          (= k \W) (recur n f (- x v) y)
          (= k \S) (recur n f x (- y v))
          (= k \R) (recur n (mod (+ f (/ v 90)) 4) x y)
          (= k \L) (recur n (mod (- f (/ v 90)) 4) x y)
          (= k \F) (recur (conj n [(direction-map f) v]) f x y)
          :else [k v f])))))



(assert (= 25 (->> test-instructions
  (map parse-instruction)
  execute)))


(assert (= 508 (->> instructions
  (map parse-instruction)
  execute)))

(defn execute-with-waypoint [instructions]
  (loop [i instructions ship-x 0 ship-y 0 waypoint-x 10 waypoint-y 1]
    (if (empty? i)
      (+ (abs ship-x) (abs ship-y))
      (let [[k v] (first i)
            n (next i)]
        (cond 
          (= k \N) (recur n ship-x ship-y waypoint-x (+ waypoint-y v))
          (= k \E) (recur n ship-x ship-y (+ waypoint-x v) waypoint-y)
          (= k \W) (recur n ship-x ship-y (- waypoint-x v) waypoint-y)
          (= k \S) (recur n ship-x ship-y waypoint-x (- waypoint-y v))
          (= k \R) (let [rotate (/ v 90)
                         [new-waypoint-x new-waypoint-y] 
                          (loop [tmp 0 waypoint-x waypoint-x waypoint-y waypoint-y]
                            (if (=  tmp rotate)
                              [waypoint-x waypoint-y]
                              (recur (inc tmp) waypoint-y (- waypoint-x))))
                         ] (recur n ship-x ship-y new-waypoint-x new-waypoint-y))
          (= k \L) (let [rotate (/ v 90)
                         [new-waypoint-x new-waypoint-y] 
                          (loop [tmp 0 waypoint-x waypoint-x waypoint-y waypoint-y]
                            (if (=  tmp rotate)
                              [waypoint-x waypoint-y]
                              (recur (inc tmp) (- waypoint-y) waypoint-x)))
                         ] (recur n ship-x ship-y new-waypoint-x new-waypoint-y))
          (= k \F) (recur n (+ ship-x (* waypoint-x v)) (+ ship-y (* waypoint-y v)) waypoint-x waypoint-y)
          :else [k v])))))


(assert (= 286 (->> test-instructions
  (map parse-instruction)
  execute-with-waypoint)))

(assert (= 30761 (->> instructions
  (map parse-instruction)
  execute-with-waypoint)))