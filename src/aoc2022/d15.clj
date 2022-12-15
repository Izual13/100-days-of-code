(ns aoc2022.d15
  (:require 
    [clojure.string :as str]
    [clj-async-profiler.core :as prof]))


(def test-input (str/split (slurp "resources/aoc2022/day15_t") #"\n"))
(def input (str/split (slurp "resources/aoc2022/day15_1") #"\n"))


test-input

(defn calc-length [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2]
    (+ (abs (- x1 x2)) (abs (- y1 y2)))))

(defn parse-input [s]
  (let [[_ a b c d] (re-matches #"Sensor at x=(.*), y=(.*): closest beacon is at x=(.*), y=(.*)" s)
        x1 (Integer/parseInt a) 
        y1 (Integer/parseInt b)
        x2 (Integer/parseInt c) 
        y2 (Integer/parseInt d)]
    {:s [x1 y1] :b [x2 y2] :l (calc-length [x1 y1] [x2 y2])}))


(assert (= {:s [214540 3768792], :b [-355567 3900317], :l 701632}
          (parse-input "Sensor at x=214540, y=3768792: closest beacon is at x=-355567, y=3900317")))

(parse-input "Sensor at x=214540, y=3768792: closest beacon is at x=-355567, y=3900317")


(defn filter-sensors [row sensors] 
  (loop [s sensors r []]
    (if (empty? s)
      r
      (let [f (first s)
            y (second (:s f))
            l (:l f)]
        (if (and (<= (- y l) row (+ y l)))
          (recur (rest s) (conj r f))
          (recur (rest s) r))))))

(defn check-point [sensors p1]
  (loop [s sensors] 
    (if (empty? s)
      false
      (let [f (first s)
            p2 (:s f)
            l (:l f)]
        (if (<= (calc-length p1 p2) l)
          true
          (recur (rest s)))))))

(check-point [{:s [2 18], :b [-2 15], :l 7}] [0 100])

(defn get-borders [max-c sensor] 
  (let [l (inc (:l sensor))
        [x y] (:s sensor)
        b1 (for [i (range (inc l))
                 :let [new-x (- (+ x l) i) 
                       new-y (- y i)]
                 :while (and (<= 0 new-x max-c) (<= 0 new-y max-c))] 
             [new-x new-y])
        b2 (for [i (range (inc l))
                 :let [new-x (- (+ x l) i) 
                       new-y (+ y i)]
                 :while (and (<= 0 new-x max-c) (<= 0 new-y max-c))] 
             [new-x new-y])
        b3 (for [i (range (inc l))
                 :let [new-x (+ (- x l) i) 
                       new-y (- y i)]
                 :while (and (<= 0 new-x max-c) (<= 0 new-y max-c))] 
             [new-x new-y])
        b4 (for [i (range (inc l))
                 :let [new-x (+ (- x l) i) 
                       new-y (+ y i)]
                 :while (and (<= 0 new-x max-c) (<= 0 new-y max-c))] 
             [new-x new-y])
        ; b1 (for [i (range (inc l))] [(- (+ x l) i) (- y i)])
        ; b2 (for [i (range (inc l))] [(- (+ x l) i) (+ y i)])
        ; b3 (for [i (range (inc l))] [(+ (- x l) i) (- y i)])
        ; b4 (for [i (range (inc l))] [(+ (- x l) i) (+ y i)])
        ] 
    (set (concat b1 b2 b3 b4))))

(defn get-borders [max-c sensor] 
  (let [l (inc (:l sensor))
        [x y] (:s sensor)
        b1 [(+ x l) y]
        b2 [(- x l) y]
        b3 [x (+ y l)]
        b4 [x (- y l)]]
    (set [ b1 b2 b3 b4])))



(assert (= #{[1 1] [0 2] [2 0]}
          (get-borders 20 {:s [0 0], :b [-2 15], :l 1})))

(get-borders 20 {:s [0 0], :b [-2 15], :l 1})

(assert (= #{[6 6] [5 3] [4 6] [5 7] [6 4] [4 4] [7 5] [3 5]}
          (get-borders 20 {:s [5 5], :b [-2 15], :l 1})))


(concat [1 2] [3 4] [5 6])


(defn count-positions [row sensors]
  (let [min-x (reduce min (map #(- (first (:s %)) (:l %)) sensors))
        max-x (reduce max (map #(+ (first (:s %)) (:l %)) sensors))
        count-b (count (set (for [s sensors
                                  :when (= (second (:b s)) row)] (:b s))))
        count-p (count (for [i (range min-x max-x)
                             :when (check-point sensors [i row])] 1))]
    
    (- count-p count-b)))



(assert (= 26 (->> test-input
                (mapv parse-input)
                (filter-sensors 10)
                (count-positions 10))))

(assert (= 5838453 (->> input
                     (mapv parse-input)
                     (filter-sensors 2000000)
                     (count-positions 2000000))))















(defn count-positions2 [max-c sensors]
  (let [borders (set (mapcat #(get-borders max-c %) sensors))
        borders (filter (fn [[x y]] (and (<= 0 x max-c) (<= 0 y max-c))) borders)
        ; _ (println borders)
        points (for [p borders
                     :when (not (check-point sensors p))] p)
        ]
    
    points))

(->> test-input
  (mapv parse-input)
  (count-positions2 20))

(defn find-points [max-c sensors]
  ; (let [points (map :s sensors) ]
  (set (concat (for [s1 sensors
                     s2 sensors
                     :let [p1 (:s s1)
                           p2 (:s s2)
                           l (calc-length p1 p2)]
                     :when (and 
                             (= (- l (:l s1) (:l s2)) 2)
                             (<= 0 (first p1) max-c) 
                             (<= 0 (second p1) max-c)
                             (<= 0 (first p2) max-c) 
                             (<= 0 (second p2) max-c)
                             (not= p1 p2))]
                 [(int (/ (+ (first p1) (first p2)) 2)) (int (/ (+ (second p1) (second p2)) 2))] ))))


(defn part2 [max-c sensors]
  (let [points (find-points max-c sensors)
        points (set (mapcat (fn [[x y]] [[x y] 
                                    [x (inc y)] [x (dec y)] 
                                    [(inc x) y] [(dec x) y]
                                    [(inc x) (inc y)] [(dec x) (dec y)]]) points))
        points (for [p points
                     :when (not (check-point sensors p))] p)] points))




(->> test-input
  (mapv parse-input)
  ; (count-positions3 4000000)
  (part2 20)
  )

#{[15 12] [11 0] [11 12] [14 4] [9 13] [14 10]}


(->> input
  (mapv parse-input)
  (part2 4000000)
  )

(let [borders [[3428967 2493192] [2121465 3703050] [2281431 3228484] [2774120 2735795] [3995854 3578627]]
      sensors (->> input
                (mapv parse-input))
      points (for [p borders
                   :when (not (check-point sensors p))] p)] points)