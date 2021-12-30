(ns  aoc2021.d17
  (:require [clojure.string :as str]))

(defn parse-coordinates [s]
  (let [[_ x1 x2 y1 y2] (re-matches #"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)" s)]
    [(Long/parseLong x1) (Long/parseLong x2)
     (Long/parseLong y1)   (Long/parseLong y2)]))


(assert (= [20 30 -10 -5] (parse-coordinates "target area: x=20..30, y=-10..-5")))

(def input-from-file-test
  (-> (slurp "resources/aoc2021/day17_t")
      (str/trim)
      (parse-coordinates)))


(def input-from-file
  (-> (slurp "resources/aoc2021/day17_1")
      (str/trim)
      (parse-coordinates)))



(defn fire [[vx vy] [tx1 tx2 ty1 ty2]] (loop [x 0 y 0 vx vx vy vy max-y 0 r false]
                                         (if (or (> x tx2) (< y ty1))
                                           (if r max-y nil)
                                           (let [new-x (+ x vx)
                                                 new-y (+ y vy)
                                                 new-vx (cond
                                                          (> vx 0) (dec vx)
                                                          (< vx 0) (inc vx)
                                                          :else 0)
                                                 new-vy (dec vy)
                                                 new-r (if (and (>= tx2 x tx1) (>= ty2 y ty1)) true r)]
                                             (recur new-x new-y new-vx new-vy (max max-y y) new-r)))))


(assert (= 3 (fire [7 2] input-from-file-test)))
(assert (nil? (fire [70 20] input-from-file-test)))

(defn part1 [input]
  (->> (for [x (range 100)
             y (range 100)] (fire [x y] input))
       (filter some?)
       (apply max)))

(assert (= 45 (time (part1 input-from-file-test))))
(assert (= 2850 (time (part1 input-from-file))))



(defn part2 [input]
  (->> (for [x (range -500 500)
             y (range -500 500)
             :when (not (nil? (fire [x y] input)))] [x y])
       (count)))

(defn part2-parallel [input]
  (let [points (for [x (range -500 500)
                     y (range -500 500)] [x y])
        results (doall (pmap #(fire % input) points))]
    (count (filter #(not (nil? %)) results))))


(assert (= 112 (time (part2 input-from-file-test))))
(assert (= 1117 (time (part2 input-from-file))))
(assert (= 1117 (time (part2-parallel input-from-file))))