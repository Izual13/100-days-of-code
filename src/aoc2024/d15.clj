(ns  aoc2024.d15
  (:require [clojure.string :as str]
            [clj-async-profiler.core :as prof]))

(def test-movements (str/split (slurp "resources/aoc2024/d15_t") #"\n\n"))
(def movements (str/split (slurp "resources/aoc2024/d15_1") #"\n\n"))

(defn parse-movements [[r-map r-movements]]
  (let [r-map (str/split r-map #"#\n#")
        r-map (subvec r-map 1 (dec (count r-map)))]
       [(mapv vec r-map) (filterv #(not= \newline %) r-movements)]
    ))

(defn direction [d]
  (case d
    \^ [-1 0]
    \v [1 0]
    \> [0 1]
    \< [0 -1]))

(defn find-start [a]
  (let [c (count a)]
    (loop [j 0 i 0]
      (cond 
        (= j c) nil
        (= i c) (recur (inc j) 0)
        (= \@ (get-in a [j i])) [j i]
        :else (recur j (inc i))))))

(defn move [d si sj r-map]
  (let [[oy ox] (direction d)
        [x y] (loop [i (+ si oy) j (+ sj ox)]
                (cond 
                  (= \. (get-in r-map [i j])) [i j]
                  (= \O (get-in r-map [i j])) (recur (+ i oy) (+ j ox))
                  :else [si sj]))]
    (if (and (= x si) (= y sj))
      [si sj r-map]
      [(+ si oy) (+ sj ox) (-> r-map
        (assoc-in [x y] \O)
        (assoc-in [(+ si oy) (+ sj ox)] \.))])))

(defn simulate[[r-map r-movements]]
  (let[[i j] (find-start r-map)
       r-map (assoc-in r-map [i j] \.)]
    (loop [rm r-movements i i j j r-map r-map]
      (if (empty? rm) 
        r-map
        (let [f (first rm)
              [i' j' r-map'] (move f i j r-map)]
          (recur (next rm) i' j' r-map'))))))

(defn calculate [a]
  (let [c (count a)]
    (loop [j 0 i 0 r 0]
      (cond 
        (= j c) r
        (= i c) (recur (inc j) 0 r)
        (= \O (get-in a [j i])) (recur j (inc i) (+ r (* 100 (inc j)) (inc i)))
        :else (recur j (inc i) r)))))

(assert (= 10092 (->> test-movements
  parse-movements
  simulate
  calculate)))

(assert (= 1490942 (->> movements
  parse-movements
  simulate
  calculate)))

