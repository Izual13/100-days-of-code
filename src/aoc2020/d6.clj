(ns  aoc2020.d6
  (:require [clojure.string :as str]))

(def test-questions (str/split (slurp "resources/aoc2020/day6_t") #"\n\n"))
(def questions (str/split (slurp "resources/aoc2020/day6_1") #"\n\n"))


(assert (= 7283 (->> questions
  (mapv #(filterv (fn [x] (not= \newline x)) %))
  (mapv set)
  (mapv count)
  (apply +))))

(assert (= 11 (->> test-questions
  (mapv #(filterv (fn [x] (not= \newline x)) %))
  
  (mapv set)
  (mapv count)
  (apply +))))

(defn count-yes [list] 
  (let [c (count (str/split list #"\n"))
        f (frequencies (filter #(not= \newline %) (vec list)))]
    (loop [f f r 0]
      (if (empty? f) 
        r
        (recur (next f) (+ r (if (= c (second (first f))) 1 0)))))))

(assert (= 6 (->> test-questions
  (mapv count-yes)
  (apply +))))


(assert (= 3520 (->> questions
  (mapv count-yes)
  (apply +))))

(assert (= [\1 \2 \3] (filterv #(not= \newline %) (vec "123\n"))))
