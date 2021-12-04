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
                                    r (rest input)] (recur r (conj result f))))))

(build-board (drop 2 input-from-file-test))


(defn build-matrix [input] (let
                            [numbers (mapv #(Integer/parseInt %)  (str/split (first input) #","))
                             boards (loop [input (drop 2 input) result []]
                                      (if (empty? input) result
                                          (if (empty? (first input))
                                            (recur (rest input) result)
                                            (let [board (build-board input)] (recur (nthrest input 5) (conj result {:win false :board board}))))))]
                             {:numbers numbers :boards boards}))

(:numbers (build-matrix input-from-file-test))


(get (get [[22 13 17 11 0] [8 2 23 4 24] [21 9 14 16 7] [6 10 3 18 5] [1 12 20 15 19]] 0) 0)

(defn calc-winner [board] (apply + (filter #(not= % -1) (flatten board))))

(assert (= 188 (calc-winner [[-1 -1 -1 -1 -1] [10 16 15 -1 19] [18 8 -1 26 20] [22 -1 13 6 -1] [-1 -1 12 3 -1]])))
(assert (= 300 (calc-winner [[22 13 17 11 0] [8 2 23 4 24] [21 9 14 16 7] [6 10 3 18 5] [1 12 20 15 19]])))
(assert (= 290 (calc-winner [[22 13 17 11 0] [8 2 23 4 24] [21 9 14 16 7] [6 -1 3 18 5] [1 12 20 15 19]])))
(assert (= 5 (count [[22 13 17 11 0] [8 2 23 4 24] [21 9 14 16 7] [6 10 3 18 5] [1 12 20 15 19]])))


(defn check-board [wrapper]
  (let [board (:board wrapper)
        is-win (:win wrapper)
        c (count board)]
    (if is-win
      false
      (loop [i 0]
        (if (= i c)
          false
          (if (or (apply = (conj (get board i) -1)) (apply = (conj (map #(get (get board %) i) (range c)) -1)))
            true
            (recur (inc i))))))))

(check-board {:board [[22 13 17 11 0]]})

(assert (= false (check-board {:board [[22 13 17 11 0] [8 2 23 4 24] [21 9 14 16 7] [6 10 3 18 5] [1 12 20 15 19]]})))
(assert (= false (check-board {:board [[2 2 2 2 2] [8 2 23 4 24] [21 9 14 16 7] [6 10 3 18 5] [1 12 20 15 19]]})))
(assert (= false (check-board {:board [[2 2 2 2 2] [2 2 23 4 24] [2 9 14 16 7] [2 10 3 18 5] [2 12 20 15 19]]})))
(assert (= true (check-board {:board [[-1 13 17 11 0] [-1 2 23 4 24] [-1 9 14 16 7] [-1 10 3 18 5] [-1 12 20 15 19]]})))
(assert (= true (check-board {:board [[22 13 17 11 0] [8 2 23 4 24] [-1 -1 -1 -1 -1] [6 10 3 18 5] [1 12 20 15 19]]})))


(defn replace-board [wrapper number]
  (let [is-win (:win wrapper) board (:board wrapper)]
    {:win is-win :board (mapv (fn [row] (mapv #(if (= number %) -1 %) row)) board)}))

(replace-board [[22 13 17 11 0] [8 2 23 4 24] [21 9 14 16 7] [6 10 3 18 5] [1 12 20 15 19]] 22)


(defn part1
  ([input] (let [matrix (build-matrix input)
                 numbers (:numbers matrix)
                 boards (:boards matrix)
                 winner (loop [head (first numbers) tail (rest numbers) b boards result {:number 0 :board []}]
                          (if (nil? head)
                            result
                            (let [new-b (map #(replace-board % head) b)
                                  leader (first (filter check-board new-b))
                                  is-win (not (nil? leader))]
                              (if is-win
                                (conj leader {:number head})
                                (recur (first tail) (rest tail) new-b result)))))]
             (* (:number winner) (calc-winner (:board winner))))))

(assert (= 4512 (part1 input-from-file-test)))
(assert (= 41668 (part1 input-from-file)))

(defn part2
  ([input] (let [matrix (build-matrix input)
                 numbers (:numbers matrix)
                 boards (:boards matrix)
                 winner (loop [head (first numbers) tail (rest numbers) b boards result {:number 0 :board []}]
                          (if (nil? head)
                            result
                            (let [new-b (mapv #(replace-board % head) b)
                                  g (group-by check-board new-b)
                                  new-b (apply conj (get g false) (mapv #(assoc % :win true) (get g true)))
                                  leader (first (get g true))
                                  is-win (not (nil? leader))]
                              (if is-win
                                (recur (first tail) (rest tail) new-b {:number head :board (:board leader)})
                                (recur (first tail) (rest tail) new-b result)))))]
             (* (:number winner) (calc-winner (:board winner))))))

(assert (= 1924 (part2 input-from-file-test)))
(assert (= 10478 (part2 input-from-file)))

