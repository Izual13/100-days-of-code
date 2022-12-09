(ns aoc2022.d9
  (:require 
    [clojure.string :as str]
    [clj-async-profiler.core :as prof]))


(def test-input (clojure.string/split (slurp "resources/aoc2022/day9_t") #"\n"))
(def input (clojure.string/split (slurp "resources/aoc2022/day9_1") #"\n"))

input

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

(is-close [3 1] [4 0])
(is-close [3 1] [3 1])


(defn calc-tail-2 [current-position tail-position distination length]
  (loop [cp current-position tp tail-position l length r #{}]
    (if (= l 0) 
      {:cp cp :tp tp :r r}  
      (let [[x1 y1] cp
            [x2 y2] tp]
        (cond 
          (= "L" distination) (recur [(dec x1)  y1] (if (is-close [(dec x1) y1] tp) tp cp) (dec l) (if (is-close [(dec x1) y1] tp) r (conj r cp)))
          (= "R" distination) (recur [(inc x1)  y1] (if (is-close [(inc x1) y1] tp) tp cp) (dec l) (if (is-close [(inc x1) y1] tp) r (conj r cp)))
          (= "U" distination) (recur [x1 (inc y1)] (if (is-close [x1 (inc y1)] tp) tp cp) (dec l) (if (is-close [x1 (inc y1)] tp) r (conj r cp)))
          (= "D" distination) (recur [x1 (dec y1)] (if (is-close [x1 (dec y1)] tp) tp cp) (dec l) (if (is-close [x1 (dec y1)] tp) r (conj r cp))))))))
(abs -1)

(rest [1 2 3])

(let [[x y] nil] x)

(calc-tail-2 [0 0] nil "R" 5)
(calc-tail-2 [1 3] [2 4] "R" 4)
(calc-tail-2 [4 0] [3 0] "U" 4)

(clojure.set/union #{[0 0]} #{[1 0]})

(defn count-positions [m] 
  (println "\n\n\n\n\n")
  (loop [m m ch [0 0] ct nil r #{[0 0]}]
    ; (println (first m) ch r)
    (if (empty? m) 
      {:c ch :r r}
      (let [[d l] (first m)
            [x y] ch
            [x2 y2] ct
            tail (calc-tail-2 ch ct d l)
            new-r (clojure.set/union r (:r tail))
            ;bug
            new-ct (:tp tail)
            new-ch (:cp tail)
            
            ; _ (println "ct" ct new-ct)
            ]
        (cond 
          (= "L" d) (recur (rest m) new-ch new-ct new-r)
          (= "R" d) (recur (rest m) new-ch new-ct new-r)  
          (= "U" d) (recur (rest m) new-ch new-ct new-r)
          (= "D" d) (recur (rest m) new-ch new-ct new-r))))))

(->> test-input
  (map parse-moves)
  count-positions
  :r
  sort
  count)


(assert (= 5874 (->> input
                  (map parse-moves)
                  count-positions
                  :r
                  count)))

      

..##..
...##.
.####.
....#.
s###..
   
   
[2 4] [3 4] 
[3 3] [4 3]  
[0 2]       [2 2] [3 2] [4 2]     
[1 2]             [4 1]
([0 0] [1 0] [2 0] [3 0] )




























(assert (= 1792 (let[m input
                     y (count m)
                     x (count (first m))]
                  (+ y x x y -4 (count (for [i (range 1 (dec x))
                                             j (range 1 (dec y))
                                             :let [e (get-in m [i j])]
                                             :when (or 
                                                     (every? #(> e (get-in m [% j])) (range 0 i))
                                                     (every? #(> e (get-in m [% j])) (range (inc i) x))
                                                     (every? #(> e (get-in m [i %])) (range 0 j))
                                                     (every? #(> e (get-in m [i %])) (range (inc j) y)))]
                                         [i j]))))))



(assert (= 8 (let[m test-input
                  y (count m)
                  x (count (first m))]
               (reduce max (for [i (range 1 (dec x))
                                 j (range 1 (dec y))
                                 :let [e (get-in m [j i])
                                       s1 (for [i2 (range (dec i) -1 -1) :while (> e (get-in m [j i2]))]  1)
                                       s2 (for [i2 (range (inc i) x) :while (> e (get-in m [j i2]))]  [j i2 e (get-in m [i2 j])])
                                       s3 (for [j2 (range (dec j) -1 -1) :while (> e (get-in m [j2 i]))]  1)
                                       s4 (for [j2 (range (inc j) y) :while (> e (get-in m [j2 i]))] [j2 i (get-in m [j2 i])])
              
                                       s1 (+ (count s1) (if (= (count s1) i) 0 1))
                                       s2 (+ (count s2) (if (= (count s2) (- x i 1)) 0 1))
                                       s3 (+ (count s3) (if (= (count s3) j) 0 1))
                                       s4 (+ (count s4) (if (= (count s4) (- y j 1)) 0 1))]]
                             (* s1 s2 s3 s4))))))

(assert (= 334880 (let[m input
                       y (count m)
                       x (count (first m))
                       ]
                    (reduce max (for [i (range 1 (dec x))
                                      j (range 1 (dec y))
                                      :let [e (get-in m [j i])
                                            s1 (for [i2 (range (dec i) -1 -1) :while (> e (get-in m [j i2]))]  1)
                                            s2 (for [i2 (range (inc i) x) :while (> e (get-in m [j i2]))]  [j i2 e (get-in m [i2 j])])
                                            s3 (for [j2 (range (dec j) -1 -1) :while (> e (get-in m [j2 i]))]  1)
                                            s4 (for [j2 (range (inc j) y) :while (> e (get-in m [j2 i]))] [j2 i (get-in m [j2 i])])
              
                                            s1 (+ (count s1) (if (= (count s1) i) 0 1))
                                            s2 (+ (count s2) (if (= (count s2) (- x i 1)) 0 1))
                                            s3 (+ (count s3) (if (= (count s3) j) 0 1))
                                            s4 (+ (count s4) (if (= (count s4) (- y j 1)) 0 1))]]
                                  (* s1 s2 s3 s4))))))

