(ns  aoc2024.d13
  (:require [clojure.string :as str]
            [clj-async-profiler.core :as prof]))

(def test-configuration (str/split (slurp "resources/aoc2024/d13_t") #"\n\n"))
(def configuration (str/split (slurp "resources/aoc2024/d13_1") #"\n\n"))

(defn parse-configuration [c]
  (let [[r1 r2 r3] (str/split c #"\n")
        [_ ax ay] (re-matches #"Button A: X\+(\d*), Y\+(\d*)" r1)
        [_ bx by] (re-matches #"Button B: X\+(\d*), Y\+(\d*)" r2)
        [_ px py] (re-matches #"Prize: X=(\d*), Y=(\d*)" r3)] 
        [(Integer/parseInt ax) (Integer/parseInt ay) (Integer/parseInt bx) (Integer/parseInt by) (Integer/parseInt px) (Integer/parseInt py)]))

(defn calculate [[ax ay bx by px py]]
  (let [B (/ (- (* ax py) (* px ay)) (- (* ax by) (* bx ay)))
        A (/ (- (* bx py) (* px by)) (- (* bx ay) (* ax by)))]
    (when (and (not (ratio? A)) (not (ratio? B)))
      (+ (* 3 A) B))))

(assert (= 480 (->> test-configuration
  (mapv parse-configuration)
  (mapv calculate)
  (filterv some?)
  (apply +))))

(assert (= 35574 (->> configuration
  (mapv parse-configuration)
  (mapv calculate)
  (filterv some?)
  (apply +))))

(assert (= 80882098756071 (->> configuration
  (mapv parse-configuration)
  (mapv #(-> % 
           (update 4 (fn [x] (+ 10000000000000 x)))
           (update 5 (fn [x] (+ 10000000000000 x)))))
  (mapv calculate)
  (filterv some?)
  (apply +))))