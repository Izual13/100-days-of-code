(ns aoc2022.d9
  (:require 
    [clojure.string :as str]
    [clj-async-profiler.core :as prof]))


(def test-input (clojure.string/split (slurp "resources/aoc2022/day9_t") #"\n"))
(def test-input2 (clojure.string/split (slurp "resources/aoc2022/day9_t2") #"\n"))
(def input (clojure.string/split (slurp "resources/aoc2022/day9_1") #"\n"))

(defn parse-moves [s]
  (let [[_ a b] (re-matches #"(\w) (\d*)" s)]
    [a (Integer/parseInt b)]))


(assert (= ["U" 123] (parse-moves "U 123")))


(defn is-close [[x1 y1] [x2 y2]]
  (if (or (nil? x1) (nil? x2) (nil? y1) (nil? y2))
    false
    (cond
      (and (= x1 x2) (= y1 y2)) true
      (and (=  x1 x2) (= 1 (abs (- y1 y2)))) true
      (and (=  y1 y2) (= 1 (abs (- x1 x2)))) true
      (and (= 1 (abs (- x1 x2))) (= 1 (abs (- y1 y2)))) true
      :else false)))

(assert (= true (is-close [3 1] [4 0])))
(assert (= true (is-close [3 1] [3 1])))


(defn calc-tail [current-position tail-position distination length]
  (loop [cp current-position tp tail-position l length r #{}]
    (if (= l 0) 
      {:cp cp :tp tp :r r}  
      (let [[x1 y1] cp
            [x2 y2] tp
            ic (fn [p r1 r2] (if (is-close p tp) r1 r2))]
        (cond 
          (= "L" distination) (recur [(dec x1) y1] (ic [(dec x1) y1] tp cp) (dec l) (ic [(dec x1) y1] r (conj r cp)))
          (= "R" distination) (recur [(inc x1) y1] (ic [(inc x1) y1] tp cp) (dec l) (ic [(inc x1) y1] r (conj r cp)))
          (= "U" distination) (recur [x1 (inc y1)] (ic [x1 (inc y1)] tp cp) (dec l) (ic [x1 (inc y1)] r (conj r cp)))
          (= "D" distination) (recur [x1 (dec y1)] (ic [x1 (dec y1)] tp cp) (dec l) (ic [x1 (dec y1)] r (conj r cp))))))))

(assert (= {:cp [5 0], :tp [4 0], :r #{[0 0] [1 0] [3 0] [2 0] [4 0]}} (calc-tail [0 0] nil "R" 5)))
(assert (= {:cp [5 3], :tp [4 3], :r #{[4 3] [3 3]}} (calc-tail [1 3] [2 4] "R" 4)))
(assert (= {:cp [4 4], :tp [4 3], :r #{[4 3] [4 2] [4 1]}} (calc-tail [4 0] [3 0] "U" 4)))

(defn count-positions [m] 
  (loop [m m ch [0 0] ct nil r #{[0 0]}]
    (if (empty? m) 
      {:c ch :r r}
      (let [[d l] (first m)
            [x y] ch
            [x2 y2] ct
            tail (calc-tail ch ct d l)]
        (recur (rest m) (:cp tail) (:tp tail) (clojure.set/union r (:r tail)))))))

(assert (= 13 (->> test-input
                (map parse-moves)
                count-positions
                :r
                sort
                count)))


(assert (= 5874 (->> input
                  (map parse-moves)
                  count-positions
                  :r
                  count)))


(defn next-point [[x y] d]
  (case d
    "L" [(dec x) y]
    "R" [(inc x) y]
    "U" [x (inc y)]
    "D" [x (dec y)]))

(defn recalc-tails [t] 
  (let [c (count t) ] 
    (loop [i 1 r t] 
      (if (= i c)
        r
        (let [[x1 y1] (r (dec i))
              [x2 y2] (r i)
              new-t (if (and (<= (abs (- x1 x2)) 1) (<= (abs (- y1 y2)) 1)) 
                      [x2 y2]
                      [(+ x2 (Integer/signum (- x1 x2))) (+ y2 (Integer/signum (- y1 y2)))])]
          (recur (inc i) (assoc r i new-t)))))))

(defn calc-tails [cp ropes d l]
  (loop [cp cp ropes ropes i 0 result #{[0 0]}]
    (if (= l i) 
      {:cp cp :ropes ropes :r result}  
      (let [[x y] cp
            np (next-point cp d)]
        (recur np (recalc-tails (assoc ropes 0 np)) (inc i) (conj result (last ropes)))))))


(defn count-ropes [m] 
  (loop [m m cp [0 0] ropes (vec (repeat 10 [0 0])) result #{[0 0]} ]
    (if (empty? m) 
      (conj result (last ropes))
      (let [[d l] (first m)
            new-result (calc-tails cp ropes d l)]
        (recur (rest m) (:cp new-result) (:ropes new-result) (clojure.set/union result (:r new-result)))))))



(assert (= 36 (->> test-input2
                (map parse-moves)
                count-ropes
                count)))


(assert (= 2467 (->> input
                  (map parse-moves)
                  count-ropes
                  count)))


