(ns  aoc2021.d4
  (:require [clojure.string :as str]))

(def input-from-file-test
  (str/split-lines
   (slurp "resources/aoc2021/day4_t")))

(def input-from-file
  (str/split-lines
   (slurp "resources/aoc2021/day4_1")))


(defn build-board [input] (loop [input (take 5 input) result []]
                           (if (empty? input)
                             result
                             (let [f (mapv #(Integer/parseInt %) (str/split (str/trim (first input)) #"\s+"))
                                   _ (println f)
                                   r (rest input)] (recur r (conj result f))))))

(build-board (drop 2 input-from-file-test))


(defn build-matrix [input] (let
                            [numbers (map #(Integer/parseInt %)  (str/split (first input) #","))
                             boards (loop [input (drop 2 input) result []] 
                                      (if (empty? input) result
                                          (if (empty? (first input)) 
                                            (recur (rest input) result)
                                            (let [board (build-board input)] (recur (nthrest input 5) (conj result {:win false :board board}))))))]
                             {:numbers numbers :boards boards}))

(build-matrix input-from-file-test)

(->> (str/split (str/trim " 6 10  3 18  5") #"\s+")
     (mapv #(Integer/parseInt %)))

(get (get ['(1 2) '(2 3)] 0) 0)

(get (get [[1 2] [1 2]] 0)0)

(defn part1
  ([input] (let [c (count input)
                 h (/ c 2)
                 cw (count (first input))
                 g (->> (loop [i 0 r []]
                          (if (= i cw)
                            r
                            (recur (inc i) (conj r (count (filter #(= % \1)  (map #(nth % i) input)))))))
                        (map #(if (> % h) 1 0)))
                 gamma (Integer/parseInt (apply str g) 2)
                 epsilon (Integer/parseInt (apply str (map #(if (= 1 %) 0 1) g)) 2)]
             (* gamma epsilon))))




(assert (= 230 (part1 input-from-file-test)))
(assert (= 4406844 (part1 input-from-file)))

