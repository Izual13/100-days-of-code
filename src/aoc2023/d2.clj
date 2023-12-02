(ns  aoc2023.d2
  (:require [clojure.string :as str]))

(def games (clojure.string/split (slurp "resources/aoc2023/day2_1") #"\r?\n"))

(def games-t (clojure.string/split (slurp "resources/aoc2023/day2_t") #"\r?\n"))


(defn parse-game [s]
  (let [[_ id parts] (re-matches #"Game (\d*): (.*)" s)
        parts (clojure.string/split parts #"; ")
        parts (map #(clojure.string/split %1 #", ") parts)]
    [(Integer/parseInt id) (vec (flatten parts))]))


(defn check-game [red green blue [id parts]]
  (loop [p parts r 0 g 0 b 0]
    (cond 
      (or (> r red) (> g green) (> b blue)) false
      (empty? p) true
      :else (let [f (first p)
            [_ cnt clr] (re-matches #"(\d*) (.*)" f)]
        (cond 
          (= "red" clr)   (recur (next p) (Integer/parseInt cnt) g b)
          (= "green" clr) (recur (next p) r (Integer/parseInt cnt) b)
          (= "blue" clr)  (recur (next p) r g (Integer/parseInt cnt))
          :else (recur (next p) r g b))))))

(defn check-game-v2 [[id parts]]
  (loop [p parts r 0 g 0 b 0]
    (cond 
      (empty? p) (* r g b)
      :else (let [f (first p)
            [_ cnt clr] (re-matches #"(\d*) (.*)" f)
            ]
        (cond 
          (= "red" clr)   (recur (next p) (max r (Integer/parseInt cnt)) g b)
          (= "green" clr) (recur (next p) r (max g (Integer/parseInt cnt)) b)
          (= "blue" clr)  (recur (next p) r g (max b (Integer/parseInt cnt)))
          :else (recur (next p) r g b))))))


(assert (= false
          (check-game 12 13 14 [95 ["7 blue" "14 red" "9 blue" "17 red" "2 blue" "1 green" "4 red"]])))

(assert (= true
          (check-game 12 13 14 [1 ["3 blue" "4 red" "1 red" "2 green" "6 blue" "2 green"]])))

(assert (= 48
          (check-game-v2 [1 ["3 blue" "4 red" "1 red" "2 green" "6 blue" "2 green"]])))


(assert (= 8 (->> 
  games-t
  (map parse-game)
  (filter #(check-game 12 13 14 %1))
  (map first)
  (apply +))))

(assert (= 2286 (->> 
  games-t
  (map parse-game)
  (map check-game-v2)
  (apply +))))


(assert (= 2162 (->> 
  games
  (map parse-game)
  (filter #(check-game 12 13 14 %1))
  (map first)
  (apply +))))

(assert (= 72513 (->> 
  games
  (map parse-game)
  (map check-game-v2)
  (apply +))))


