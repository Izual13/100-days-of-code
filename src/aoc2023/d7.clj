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

(defn get-card [h i]
  (get cards (nth h i)))

(defn compare-puzzle [[f _] [s _]] 
  (let [ff (sort compare-frequencies (frequencies f))
        sf (sort compare-frequencies (frequencies s))
        r (cond 
            (> (second (last ff)) (second (last sf))) 1
            (< (second (last ff)) (second (last sf))) -1
            (< (count ff) (count sf)) 1
            (> (count ff) (count sf)) -1
            (> (get-card f 0) (get-card s 0)) 1
            (< (get-card f 0) (get-card s 0)) -1
            
            (> (get-card f 1) (get-card s 1)) 1
            (< (get-card f 1) (get-card s 1)) -1
            
            (> (get-card f 2) (get-card s 2)) 1
            (< (get-card f 2) (get-card s 2)) -1
            
            (> (get-card f 3) (get-card s 3)) 1
            (< (get-card f 3) (get-card s 3)) -1
            
            (> (get-card f 4) (get-card s 4)) 1
            (< (get-card f 4) (get-card s 4)) -1
            
            (> (get-card f 5) (get-card s 5)) 1
            (< (get-card f 5) (get-card s 5)) -1
            :else 0
            )]
    
    r))

(assert (= `(["2AAAA" 2] ["33332" 1]) (sort compare-puzzle [["33332" 1] ["2AAAA" 2]])))
(assert (= `(["KJJJT" 2] ["23332" 1]) (sort compare-puzzle [["23332" 1] ["KJJJT" 2]])))
(assert (= `(["KTJJT" 2] ["QQQJA" 1]) (sort compare-puzzle [["QQQJA" 1] ["KTJJT" 2]])))
(assert (= `(["T55J5" 2] ["QQQJA" 1]) (sort compare-puzzle [["QQQJA" 1] ["T55J5" 2]])))
(assert (= `(["KTJJT" 2] ["KK677" 1]) (sort compare-puzzle [["KK677" 1] ["KTJJT" 2]])))
(assert (= `(["KK677" 1] ["T55J5" 2]) (sort compare-puzzle [["KK677" 1] ["T55J5" 2]])))





(defn calc [s] 
  (loop [s s i 1 r 0]
    (if (empty? s) 
      r
      (recur (next s) (inc i) (+ r (* i (first s)))))))

(assert (= 6440 (->> puzzles-t
                  (map parse-puzzle)
                  (sort compare-puzzle)
                  (map second)
                  calc)))

(assert (= 249748283 (->> puzzles
                       (map parse-puzzle)
                       (sort compare-puzzle)
                       (map second)
                       calc)))

;;;;;part 2

(def cards-v2 {\A 14, \K 13, \Q 12, \J 1, \T 10, \9 9, \8 8, \7 7, \6 6, \5 5, \4 4, \3 3, \2 2})

(defn get-card-v2 [h i]
  (get cards-v2 (nth h i)))

(defn compare-frequencies-v2 [[w1 c1] [w2 c2]] 
  (cond 
    (> c1 c2) 1
    (< c1 c2) -1
    :else (compare (get cards-v2 w1) (get cards-v2 w2))))

(defn replace-j [f]
  (let [ff (frequencies f)
        j (get ff \J)        
        ff (dissoc ff \J)      
        sff (sort compare-frequencies-v2 ff)]           
    (cond 
      (nil? j) sff
      (empty? ff) (assoc ff \A 5)
      :else (sort compare-frequencies-v2 (update ff (first (last sff)) #(+ j %1))))))

(defn compare-puzzle-v2 [[f _] [s _]] 
  (let [ff (replace-j f)
        sf (replace-j s)        
        r (cond 
            (> (second (last ff)) (second (last sf))) 1
            (< (second (last ff)) (second (last sf))) -1
            (< (count ff) (count sf)) 1
            (> (count ff) (count sf)) -1
            (> (get-card-v2 f 0) (get-card-v2 s 0)) 1
            (< (get-card-v2 f 0) (get-card-v2 s 0)) -1
            (> (get-card-v2 f 1) (get-card-v2 s 1)) 1
            (< (get-card-v2 f 1) (get-card-v2 s 1)) -1
            (> (get-card-v2 f 2) (get-card-v2 s 2)) 1
            (< (get-card-v2 f 2) (get-card-v2 s 2)) -1
            (> (get-card-v2 f 3) (get-card-v2 s 3)) 1
            (< (get-card-v2 f 3) (get-card-v2 s 3)) -1
            (> (get-card-v2 f 4) (get-card-v2 s 4)) 1
            (< (get-card-v2 f 4) (get-card-v2 s 4)) -1
            (> (get-card-v2 f 5) (get-card-v2 s 5)) 1
            (< (get-card-v2 f 5) (get-card-v2 s 5)) -1
            :else 0
            )]
    
    r))

(assert (= 5905 (->> puzzles-t
                  (map parse-puzzle)
                  (sort compare-puzzle-v2)
                  (map second)
                  calc)))

(assert (= 248029057 (->> puzzles
                       (map parse-puzzle)
                       (sort compare-puzzle-v2)
                       (map second)
                       calc)))

