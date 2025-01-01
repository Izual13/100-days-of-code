(ns  aoc2024.d16
  (:require [clojure.string :as str]
            [clj-async-profiler.core :as prof])
   (:use     [utils]))


(def test-maze (str/split (slurp "resources/aoc2024/d16_t") #"\n"))
(def test-maze2 (str/split (slurp "resources/aoc2024/d16_t2") #"\n"))
(def maze (str/split (slurp "resources/aoc2024/d16_1") #"\n"))

(utils/find-start [[1 2 3 4]] 4)

(defn direction [d]
  (case d
    \^ [-1 0]
    \v [1 0]
    \> [0 1]
    \< [0 -1]))

(defn find-neighbours [i j m cd cr v]
  (let [check (fn [i' j' r' d'] (and (not (= (get-in m [i' j']) \#)) (or (nil? (v [i' j' d'])) (< r' (v [i' j' d'])))))]
    (case cd
      \^ (for [[oy ox r nd] [[-1 0 0 \^][1 0 2000 \v][0 1 1000 \>][0 -1 1000 \<]]
               :let [i' (+ i oy)
                     j' (+ j ox)
                     r' (+ cr r 1)]
               :when (check i' j' r' nd)] [i' j' nd r'])
      \v (for [[oy ox r nd] [[-1 0 2000 \^][1 0 0 \v][0 1 1000 \>][0 -1 1000 \<]]
               :let [i' (+ i oy)
                     j' (+ j ox)
                     r' (+ cr r 1)]
               :when (check i' j' r' nd)] [i' j' nd r'])
      \> (for [[oy ox r nd] [[-1 0 1000 \^][1 0 1000 \v][0 1 0 \>][0 -1 2000 \<]]
               :let [i' (+ i oy)
                     j' (+ j ox)
                     r' (+ cr r 1)]
               :when (check i' j' r' nd)] [i' j' nd r'])
      \< (for [[oy ox r nd] [[-1 0 1000 \^][1 0 1000 \v][0 1 2000 \>][0 -1 0 \<]]
               :let [i' (+ i oy)
                     j' (+ j ox)
                     r' (+ cr r 1)]
               :when (check i' j' r' nd)] [i' j' nd r'])
    )))

(defn dijkstra [m]
  (let [[si sj] (utils/find-start m \S)
        [ei ej] (utils/find-start m \E)]
    (loop [points #{[si sj \< 0]} v {}]
      (if (empty? points)
        (apply min (for [i [\^ \< \> \v]
                         :let [v' (v [ei ej i])]
                         :when (not (nil? v'))] v'))
        (let [[i j d r] (first points)
              neighbours (find-neighbours i j m d r v)
              points' (disj points [i j d r])
              tmp (v [i j d])]
          (recur (apply conj points' neighbours) (if (or (nil? tmp) (< r tmp)) (assoc v [i j d] r) v)))))))


(disj #{[1 23]} [1 3])

(->> test-maze2
  (mapv vec)
  dijkstra)

(time (->> test-maze2
  (mapv vec)
  dijkstra))


(->> maze
  (mapv vec)
  dijkstra)

(compare 1 2)

(sorted-set-by (fn [a b] (compare a b)) [[1 2 3 4] [4 5 6 7]])

(->> test-maze
  (mapv vec)
  dijkstra)


(assert (= 7036 (->> test-maze
  (mapv vec)
  dijkstra)))

(assert (= 11048 (->> test-maze2
  (mapv vec)
  dijkstra)))

(assert (= 160624 (->> maze
  (mapv vec)
  dijkstra)))

(defn dijkstra2 [m]
  (let [[si sj] (utils/find-start m \S)
        [ei ej] (utils/find-start m \E)]
    (loop [points #{[si sj \< 0]} v {}]
      (if (empty? points)
        [m v]
        (let [[i j d r] (first points)
              neighbours (find-neighbours i j m d r v)
              points' (disj points [i j d r])
              tmp (v [i j d])]
          (recur (apply conj points' neighbours) (if (or (nil? tmp) (< r tmp)) (assoc v [i j d] r) v)))))))

(defn find-min [v i j]
  (loop [d [\^ \< \> \v] m Integer/MAX_VALUE r []]
    (cond 
      (empty? d) r
      (nil? (v [i j (first d)])) (recur (next d) m r)
      (< (v [i j (first d)]) m) (recur (next d) (v [i j (first d)]) [[i j (first d)]])
      (= (v [i j (first d)]) m) (recur (next d) m (conj r [i j (first d)]))
      :else (recur (next d) m r))))

(defn analyze [[m v]]
  (let [[ei ej] (utils/find-start m \E)
        neighbours (fn [i j d] (loop [p [[-1 0 \<][1 0 \<][0 1 \<][0 -1 \<]
                                       [-1 0 \>][1 0 \>][0 1 \>][0 -1 \>]
                                       [-1 0 \^][1 0 \^][0 1 \^][0 -1 \^]
                                       [-1 0 \v][1 0 \v][0 1 \v][0 -1 \v]] r #{}]
                               (if (empty? p) 
                                 r
                                 (let [[oi oj od] (first p)
                                       cv (v [i j d])
                                       tmp (v [(+ i oi) (+ j oj) od])]
                                   ; (if (nil? tmp) 1 (println [i j d] "->" [(+ i oi) (+ j oj) od] cv tmp))
                                   (cond
                                     (nil? tmp) (recur (next p) r)
                                     (or (= 1 (- cv tmp)) 
                                       (= 1001 (- cv tmp))
                                       ; (= 2001 (- cv tmp))
                                       ) (recur (next p) (conj r [(+ i oi) (+ j oj) od]))
                                     :else (recur (next p) r))))))]
    (loop [points (find-min v ei ej) r 0 v #{}]
      (cond 
        (empty? points) v
        (contains? v (first points)) (recur (next points) r v)
        :else (let [[i j d] (first points)
                    n (neighbours i j d)]        
                ; (println "n" n)
                (recur (apply conj (next points) n) (+ r (count n)) (conj v [i j d])))))))

(assert (= 45 (->> test-maze
  (mapv vec)
  dijkstra2
  analyze
  (map (fn [x] [(first x) (second x)]))
  set
  count)))

(assert (= 64 (->> test-maze2
  (mapv vec)
  dijkstra2
  analyze
  (map (fn [x] [(first x) (second x)]))
  set
  count)))

(assert (= 692 (->> maze
  (mapv vec)
  dijkstra2
  analyze
  (map (fn [x] [(first x) (second x)]))
  set
  count)))


(do
  (println "start profiling")
  (prof/start)
  (->> maze
  (mapv vec)
  dijkstra2
  analyze
  (map (fn [x] [(first x) (second x)]))
  set
  count)
  (println (prof/stop))
  (println "end profiling"))

