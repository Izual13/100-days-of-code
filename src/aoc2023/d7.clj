(ns  aoc2023.d7
  (:require [clojure.string :as str]))


(def puzzles (clojure.string/split (slurp "resources/aoc2023/day7_1") #"\r?\n"))

(def puzzles-t (clojure.string/split (slurp "resources/aoc2023/day7_t") #"\r?\n"))


(def cards {\A 14, \K 13, \Q 12, \J 11, \T 10, \9 9, \8 8, \7 7, \6 6, \5 5, \4 4, \3 3, \2 2})

(get cards \A)

(defn parse-puzzle [s] 
  (let [[f s] (clojure.string/split s #" ")]
    [f (Integer/parseInt s)]))

(defn compare-frequencies [[w1 c1] [w2 c2]] 
  (cond 
    (> c1 c2) 1
    (< c1 c2) -1
    :else (compare (get cards w1) (get cards w2))))



(compare 1 2)

;второе больше
(defn compare-puzzle [[f _] [s _]] 
  (println f s)
  (let [ff (sort compare-frequencies (frequencies f))
        sf (sort compare-frequencies (frequencies s))
        _ (println ff sf)
        _ (println (last ff) (last sf))
        ;_ (println (> (second (last ff)) (second (last sf))))
        r (cond 
            (> (second (last ff)) (second (last sf))) 1
            (< (second (last ff)) (second (last sf))) -1
            (> (get cards (first (last ff))) (get cards (first (last sf)))) 1
            (< (get cards (first (last ff))) (get cards (first (last sf)))) -1
            :else 0
            )
        _ (println "result:" r)]
    
    r))

(compare-puzzle ["QQQJA" 483] ["KTJJT" 220])

(compare-puzzle ["QQQJA" 483] ["T55J5" 684])

(compare-puzzle ["KK677" 28] ["KTJJT" 220])

(compare-puzzle ["KK677" 28] ["T55J5" 684])

(compare 1 2)

(get cards (first [\Q 3]))

(max-by second {T 1, 5 3, J 1})

(println "\n\n\n\n")

(defn calc [s] 
  (loop [s s i 1 r 0]
    (if (empty? s) 
      r
      (recur (next s) (inc i) (+ r (* i (first s)))))))

(->> puzzles-t
  (map parse-puzzle)
  (sort compare-puzzle)
  (map second)
  calc)

(->> puzzles
  (map parse-puzzle)
  (sort compare-puzzle)
  (map second)
  calc)


;;; tried 249600020


(["32T3K" 765] ["KTJJT" 220] ["KK677" 28] ["T55J5" 684] ["QQQJA" 483])










