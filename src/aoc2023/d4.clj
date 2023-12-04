(ns  aoc2023.d4
  (:require [clojure.string :as str]))


(def cards (clojure.string/split (slurp "resources/aoc2023/day4_1") #"\r?\n"))

(def cards-t (clojure.string/split (slurp "resources/aoc2023/day4_t") #"\r?\n"))


(defn parse-card [c] 
  (let [[_ id w n] (re-matches #"Card +(\d*): (.*) \| (.*)" c)
        w (clojure.string/split (.trim w) #" +")
        n (clojure.string/split (.trim n) #" +")
        ]
    [(Integer/parseInt id) w n]))


(defn counting [[id w n]]
  (loop [w w r 0]
    (if (empty? w) 
      (if (> r 0)
        (bit-shift-left 1 (dec r))
        r)
      (let [f (first w)]
        (if (.contains n f) 
          (recur (next w) (inc r)) 
          (recur (next w) r))))))




(assert (= 8 (counting [1 ["41" "48" "83" "86" "17"] ["83" "86" "6" "31" "17" "9" "48" "53"]])))



(assert (= 13 (->> cards-t
                (map parse-card)
                (map counting)
                (apply +)
                )))

(assert (= 24542 (->> cards
                   (map parse-card)
                   (map counting)
                   (apply +))))



(assert (= [40 ["7"] ["62"]] (parse-card "Card  40:  7  |   62")))


(defn counting-v2 [w n]
  (loop [w w r 0]
    (if (empty? w) 
      r
      (let [f (first w)]
        (if (.contains n f) 
          (recur (next w) (inc r)) 
          (recur (next w) r))))))

(defn update-r [result index score]
  (loop [r result s score]
    (if (= s 0)
      r
      (recur (update-in r [(+ (dec index) s)] #(+ (get r (dec index)) %1)) (dec s)))))

(assert (= [1 2 2 2 2 1 1 1 1 1] (update-r (vec (for [i (range 10)] 1)) 1 4)))

(defn count-cards [cards] 
  (loop [c cards r (vec (for [i (range (count cards))] 1))]
    (if (empty? c) r
      (let [[id w n] (first c)
            s (counting-v2 w n)]
        (recur (next c) (update-r r id s))))))

(assert (= 30 (->> cards-t
                (map parse-card)
                count-cards
                (apply +))))


(assert (= 8736438 (->> cards
                     (map parse-card)
                     count-cards
                     (apply +))))

