(ns  aoc2023.d3
  (:require [clojure.string :as str]))

(def schematic (clojure.string/split (slurp "resources/aoc2023/day3_1") #"\r?\n"))

(def schematic-t (clojure.string/split (slurp "resources/aoc2023/day3_t") #"\r?\n"))

(defn is-engine [p i j]
  (let [points [(get-in p [(dec i) (dec j)]) (get-in p [i (dec j)]) (get-in p [(inc i) (dec j)])
                (get-in p [(dec i) j]) (get-in p [(inc i) j])
                (get-in p [(dec i) (inc j)]) (get-in p [i (inc j)]) (get-in p [(inc i) (inc j)])]]
    (not (every? #(or (nil? %1) (= %1 \.) (Character/isDigit %1)) points))))
  

(defn find-parts [s] 
  (let [c (count s)]
    (loop [i 0 j 0 n "" is-e false r []]
    		(let [p (get-in s [j i])]
        (cond 
          (and (= j c) is-e (not (empty? n))) (conj r (Integer/parseInt n))
          (and (= j c) (not is-e)) r
          ;;;;
          (and (= i c) is-e (not (empty? n))) (recur 0 (inc j) "" false (conj r (Integer/parseInt n)))
          (and (= i c) (not is-e)) (recur 0 (inc j) "" false r)
          ;;;;
          (Character/isDigit p) (recur (inc i) j (str n p) (or is-e (is-engine s j i)) r)
          (and (not (empty? n)) is-e) (recur (inc i) j "" false (conj r (Integer/parseInt n)))
          :else (recur (inc i) j "" false r))))))



(assert (= 4361 (->> schematic-t
               	  (mapv vec)
                  find-parts
                  (apply +))))

(assert (= 544433 (->> schematic
                 	  (mapv vec)
                    find-parts
                    (apply +))))



(assert (= false (is-engine [[\4 \6 \7 \. \. \1 \1 \4 \. \.]
                             [\. \. \. \* \. \. \. \. \. \.]] 0 0)))


(assert (= false (is-engine [[\4 \6 \7 \. \. \1 \1 \4 \. \.]
                             [\. \. \. \* \. \. \. \. \. \.]] 1 0)))

(assert (= false (is-engine [[\4 \6 \7 \. \. \1 \1 \4 \. \.]
                             [\. \. \. \* \. \. \. \. \. \.]] 2 0)))


(defn find-number [p [i j]] 
  (loop [n (str (get-in p [i j])) s (dec j) e (inc j)]
    (let [l (get-in p [i s])
          r (get-in p [i e])]
      (cond 
        (and (nil? l) (nil? r)) (Integer/parseInt n)
        (and (not (nil? l)) (Character/isDigit l)) (recur (str l n) (dec s) e)
        (and (not (nil? r)) (Character/isDigit r)) (recur (str n r) s (inc e))
        :else (Integer/parseInt n)))))

(defn find-gear [p i j]
  (let [points [[(dec j) (dec i) ] [(dec j) i ] [(dec j) (inc i) ]
                [j (dec i)] [j (inc i)]
                [(inc j) (dec i)] [(inc j) i] [(inc j) (inc i)]]]
    (loop [points points r #{}] 
      (if (empty? points) 
        (if (= 1 (count r)) 
          0
          (apply * r))
        (let [f (first points)]
          (if (Character/isDigit (get-in p f))
            (recur (next points) (conj r (find-number p f)))
            (recur (next points) r)))))))

(defn find-gears [s] 
  (let [c (count s)]
    (loop [i 0 j 0 r []]
  		  (let [p (get-in s [j i])]
        (cond 
          (= j c) (apply + r)
          (= i c) (recur 0 (inc j) r)
          (= p \*) (recur (inc i) j (conj r (find-gear s i j)))
          :else (recur (inc i) j r))))))


(assert (= 467835 (->> schematic-t
                    (mapv vec)
                    find-gears)))

(time (->> schematic
        (mapv vec)
        find-gears))

(assert (= 76314915 (->> schematic
                      (mapv vec)
                      find-gears)))


