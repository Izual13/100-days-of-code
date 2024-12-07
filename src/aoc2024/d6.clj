(ns  aoc2024.d6
  (:require [clojure.string :as str]))


(def test-guard-map (str/split (slurp "resources/aoc2024/d6_t") #"\n"))
(def guard-map (str/split (slurp "resources/aoc2024/d6_1") #"\n"))


(defn find-start [m]
  (let [c (count m)]
    (loop [i 0 j 0]
      ; (println i j (get-in m [i j]))
      (cond 
        (= j c) false
        (= i c) (recur 0 (inc j))
        (= \^ (get-in m [i j])) [i j]
        :else (recur (inc i) j))))) 

(def direction {:UP [-1 0] :DOWN [1 0] :LEFT [0 -1] :RIGHT [0 1]})

(defn next-direction [d]
  (case d
    :UP :RIGHT
    :RIGHT :DOWN
    :DOWN :LEFT
    :LEFT :UP
    :else (throw (ex-info "Циклическая зависимость обнаружена"))))

(defn calculate [m]
  (let [s (find-start m)] 
    (loop [s s d :UP r 0 v #{s}]
      (if (nil? (get-in m s)) 
        r
        (let [[i j] s
              [x y] (direction d)
              new-x (+ i x)
              new-y (+ j y)
              n (get-in m [new-x new-y])]
        (cond
          (= n \.) (recur [new-x new-y] d (if (contains? v [new-x new-y]) r (inc r)) (conj v [new-x new-y]))
          (= n \^) (recur [new-x new-y] d (if (contains? v [new-x new-y]) r (inc r)) (conj v [new-x new-y]))
          (= n \#) (recur [i j] (next-direction d) r v)
          :else (count v)
          ))))))


(direction :UP)

(assert (= 41 (->> 
  test-guard-map
  (mapv vec)
  calculate)))


(assert (= 5453 (->> 
  guard-map
  (mapv vec)
  calculate)))


(def lines (str/split data #"\n"))

(def grid (mapv vec guard-map))

(def init-coord   (some (fn [[y row]]
                          (when-let [x (some (fn [[x cell]]
                                               (when (= \^ cell) x))
                                             (map-indexed  vector row))]
                            [y x]))
                        (map-indexed vector grid)))

init-coord

(defn turn [cury curx move]
  (case move
    0 [(inc cury) curx (mod (inc move) 4)]
    1 [cury (dec curx) (mod (inc move) 4)]
    2 [(dec cury) curx (mod (inc move) 4)]
    3 [cury (inc curx) (mod (inc move) 4)]))

(defn advance [cury curx move]
  (case move
    0 [(dec cury) curx move]
    1 [cury (inc curx) move]
    2 [(inc cury) curx move]
    3 [cury (dec curx) move]))

(def visited (loop [cury (first init-coord)
                    curx (second init-coord)
                    move 0
                    visited #{}]
               (if (and (>= cury 0)
                        (< cury (count grid))
                        (>= curx 0)
                        (< curx  (count (first grid))))
                 (let [[cury curx move] (if (= (get-in grid [cury curx]) \#)
                                          (turn cury curx move)
                                          [cury curx move])
                       seen' (conj visited [cury curx])
                       [cury' curx' move'] (advance cury curx move)]
                   (recur cury' curx' move' seen'))
                 visited)))

(def ans (count visited))

(def ans2 (->> visited
               (map
                (fn [[blocky blockx]]
                  (loop [ans2 0
                         grid (assoc-in grid [blocky blockx] \#)
                         cury (first init-coord)
                         curx (second init-coord)
                         move 0
                         visited #{}]
                    (if (and (>= cury 0)
                             (< cury (count grid))
                             (>= curx 0)
                             (< curx  (count (first grid)))
                             (= 0 ans2))
                      (let [ans2' (if (contains? visited [cury curx move]) 1 0)
                            [cury curx move] (if (= (get-in grid [cury curx]) \#)
                                               (turn cury curx move)
                                               [cury curx move])
                            seen' (conj visited [cury curx move])
                            [cury' curx' move'] (advance cury curx move)]
                        (recur ans2' grid cury' curx' move' seen'))
                      ans2))))
               (reduce +)))

(println ans)

(println ans2)
