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

(defn compare-puzzle [[f _] [s _]] 
  (let [ff (sort compare-frequencies (frequencies f))
        sf (sort compare-frequencies (frequencies s))
        r (cond 
            (> (second (last ff)) (second (last sf))) 1
            (< (second (last ff)) (second (last sf))) -1
            (< (count ff) (count sf)) 1
            (> (count ff) (count sf)) -1
            (> (get cards (nth f 0)) (get cards (nth s 0))) 1
            (< (get cards (nth f 0)) (get cards (nth s 0))) -1
            
            (> (get cards (nth f 1)) (get cards (nth s 1))) 1
            (< (get cards (nth f 1)) (get cards (nth s 1))) -1
            
            
            (> (get cards (nth f 2)) (get cards (nth s 2))) 1
            (< (get cards (nth f 2)) (get cards (nth s 2))) -1
            
            
            (> (get cards (nth f 3)) (get cards (nth s 3))) 1
            (< (get cards (nth f 3)) (get cards (nth s 3))) -1
            
            (> (get cards (nth f 4)) (get cards (nth s 4))) 1
            (< (get cards (nth f 4)) (get cards (nth s 4))) -1
            
            (> (get cards (nth f 5)) (get cards (nth s 5))) 1
            (< (get cards (nth f 5)) (get cards (nth s 5))) -1
            :else 0
            )]
    
    r))

(assert (= `(["2AAAA" 2] ["33332" 1]) (sort compare-puzzle [["33332" 1] ["2AAAA" 2]])))
(assert (= `(["KJJJT" 2] ["23332" 1]) (sort compare-puzzle [["23332" 1] ["KJJJT" 2]])))
(assert (= `(["KTJJT" 2] ["QQQJA" 1]) (sort compare-puzzle [["QQQJA" 1] ["KTJJT" 2]])))
(assert (= `(["T55J5" 2] ["QQQJA" 1]) (sort compare-puzzle [["QQQJA" 1] ["T55J5" 2]])))
(assert (= `(["KTJJT" 2] ["KK677" 1]) (sort compare-puzzle [["KK677" 1] ["KTJJT" 2]])))
(assert (= `(["KK677" 1] ["T55J5" 2]) (sort compare-puzzle [["KK677" 1] ["T55J5" 2]])))




(println "\n\n\n\n")

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

(defn compare-frequencies-v2 [[w1 c1] [w2 c2]] 
  (cond 
    (> c1 c2) 1
    (< c1 c2) -1
    :else (compare (get cards-v2 w1) (get cards-v2 w2))))

(defn compare-puzzle-v2 [[f _] [s _]] 
  (let [ff (frequencies f)
        sf (frequencies s)
        j-ff (get ff \J)
        j-sf (get sf \J)
        
        ff (dissoc ff \J)
        sf (dissoc sf \J)
        
        sff (sort compare-frequencies-v2 ff)
        ssf (sort compare-frequencies-v2 sf)
               
        ff (cond 
             (nil? j-ff) sff
             (empty? ff) (assoc ff \A 5)
             :else (sort compare-frequencies-v2 (update ff (first (last sff)) #(+ j-ff %1))))
        
        sf (cond 
             (nil? j-sf) ssf
             (empty? sf) (assoc sf \A 5)
             :else (sort compare-frequencies-v2 (update sf (first (last ssf)) (fnil #(+ j-sf %1) 5))))
        
        r (cond 
            (> (second (last ff)) (second (last sf))) 1
            (< (second (last ff)) (second (last sf))) -1
            (< (count ff) (count sf)) 1
            (> (count ff) (count sf)) -1
            (> (get cards-v2 (nth f 0)) (get cards-v2 (nth s 0))) 1
            (< (get cards-v2 (nth f 0)) (get cards-v2 (nth s 0))) -1
            
            (> (get cards-v2 (nth f 1)) (get cards-v2 (nth s 1))) 1
            (< (get cards-v2 (nth f 1)) (get cards-v2 (nth s 1))) -1
            
            
            (> (get cards-v2 (nth f 2)) (get cards-v2 (nth s 2))) 1
            (< (get cards-v2 (nth f 2)) (get cards-v2 (nth s 2))) -1
            
            
            (> (get cards-v2 (nth f 3)) (get cards-v2 (nth s 3))) 1
            (< (get cards-v2 (nth f 3)) (get cards-v2 (nth s 3))) -1
            
            (> (get cards-v2 (nth f 4)) (get cards-v2 (nth s 4))) 1
            (< (get cards-v2 (nth f 4)) (get cards-v2 (nth s 4))) -1
            
            (> (get cards-v2 (nth f 5)) (get cards-v2 (nth s 5))) 1
            (< (get cards-v2 (nth f 5)) (get cards-v2 (nth s 5))) -1
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

