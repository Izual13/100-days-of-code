(ns  aoc2024.d13
  (:require [clojure.string :as str]
            [clj-async-profiler.core :as prof]))



(def test-configuration (str/split (slurp "resources/aoc2024/d13_t") #"\n\n"))
(def configuration (str/split (slurp "resources/aoc2024/d13_1") #"\n\n"))

; Button A: X+30, Y+53
; Button B: X+83, Y+23
; Prize: X=3457, Y=2522

(defn parse-configuration [c]
  (let [[r1 r2 r3] (str/split c #"\n")
        [_ ax ay] (re-matches #"Button A: X\+(\d*), Y\+(\d*)" r1)
        [_ bx by] (re-matches #"Button B: X\+(\d*), Y\+(\d*)" r2)
        [_ px py] (re-matches #"Prize: X=(\d*), Y=(\d*)" r3)] 
        [(Integer/parseInt ax) (Integer/parseInt ay) (Integer/parseInt bx) (Integer/parseInt by) (Integer/parseInt px) (Integer/parseInt py)]))


(defn find-optimal-presses
  [[a-x-inc a-y-inc b-x-inc b-y-inc target-x target-y]]
  (loop [a 0
         min-presses Long/MAX_VALUE
         optimal-a 0
         optimal-b 0]
    (let [current-x (* a a-x-inc)
          current-y (* a a-y-inc)]
      (if (> current-x target-x)
        [optimal-a
         optimal-b
         min-presses]
        (let [remaining-x (- target-x current-x)
              remaining-y (- target-y current-y)]
          (if (and (>= remaining-x 0) (>= remaining-y 0)
                   (zero? (mod remaining-x b-x-inc))
                   (zero? (mod remaining-y b-y-inc)))
            (let [b-x (/ remaining-x b-x-inc)
                  b-y (/ remaining-y b-y-inc)]
              (if (= b-x b-y)
                (let [total-presses (+ a b-x)]
                  (if (< total-presses min-presses)
                    (recur (inc a) total-presses a b-x)
                    (recur (inc a) min-presses optimal-a optimal-b)))
                (recur (inc a) min-presses optimal-a optimal-b)))
            (recur (inc a) min-presses optimal-a optimal-b)))))))


  
(->> test-configuration
  (mapv parse-configuration)
  (mapv find-optimal-presses)
  (filterv (fn [[_ _ n]] (not= n Long/MAX_VALUE)))
  (mapv (fn [[x y _]] (+ (* x 3) y)))
  (apply +))

(->> configuration
  (mapv parse-configuration)
  (mapv find-optimal-presses)
  (filterv (fn [[_ _ n]] (not= n Long/MAX_VALUE)))
  (mapv (fn [[x y _]] (+ (* x 3) y)))
  (apply +))



(defn parse-configuration-2 [c]
  (let [[r1 r2 r3] (str/split c #"\n")
        [_ ax ay] (re-matches #"Button A: X\+(\d*), Y\+(\d*)" r1)
        [_ bx by] (re-matches #"Button B: X\+(\d*), Y\+(\d*)" r2)
        [_ px py] (re-matches #"Prize: X=(\d*), Y=(\d*)" r3)] 
        [(Integer/parseInt ax) (Integer/parseInt ay) (Integer/parseInt bx) (Integer/parseInt by) 
         (+ 10000000000000 (Integer/parseInt px)) 
         (+ 10000000000000 (Integer/parseInt py))]))


(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn find-optimal-presses-2
  [[a-x-inc a-y-inc b-x-inc b-y-inc target-x target-y]]
  (let [delta-x (- (* a-x-inc b-y-inc) (* b-x-inc a-y-inc))
        delta-y (- (* a-y-inc b-x-inc) (* a-x-inc b-y-inc))
        gcd-delta (gcd delta-x delta-y)
        scale-factor-x (quot target-x gcd-delta)
        scale-factor-y (quot target-y gcd-delta)
        adj-a-x-inc (* a-x-inc scale-factor-x)
        adj-a-y-inc (* a-y-inc scale-factor-x)
        adj-b-x-inc (* b-x-inc scale-factor-y)
        adj-b-y-inc (* b-y-inc scale-factor-y)]
    (loop [a 0
           solutions []]
      (let [current-x (* a adj-a-x-inc)
            current-y (* a adj-a-y-inc)
            remaining-x (- target-x current-x)
            remaining-y (- target-y current-y)]
        (if (> current-x target-x)
          (when (seq solutions)
            (apply min-key (fn [[_ _ total-presses]] total-presses) solutions))
          (if (and (>= remaining-x 0)
                   (>= remaining-y 0)
                   (zero? (mod remaining-x adj-b-x-inc))
                   (zero? (mod remaining-y adj-b-y-inc)))
            (let [b-x (quot remaining-x adj-b-x-inc)
                  b-y (quot remaining-y adj-b-y-inc)]
              (if (= b-x b-y)
                (recur (inc a) (conj solutions [a b-x (+ a b-x)]))
                (recur (inc a) solutions)))
            (recur (inc a) solutions)))))))



(->> test-configuration
  (mapv parse-configuration-2)
  (mapv find-optimal-presses-2)
  ; (filterv (fn [[_ _ n]] (not= n Long/MAX_VALUE)))
  ; (mapv (fn [[x y _]] (+ (* x 3) y)))
  ; (apply +)
  
  )