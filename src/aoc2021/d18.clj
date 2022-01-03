(ns  aoc2021.d18
  (:require [clojure.string :as str]))


(def input-from-file-test
  (->> (slurp "resources/aoc2021/day18_t")
       (str/split-lines)
       (map read-string)))


(def input-from-file
  (->> (slurp "resources/aoc2021/day18_1")
       (str/split-lines)
       (map read-string)))

(assert (= [[3,8],7] (read-string "[[3,8],7]")))

(defn left [x] (first x))
(defn right [x] (second x))

(assert (= 1 (left [1 2])))
(assert (= 2 (right [1 2])))

(defn replace-last [v n]
  (loop [v v]
    (if (empty? v)
      nil
      (if (= n (peek v))
        (-> (pop v)
            (conj (- 1 n)))

        (recur (pop v))))))

(replace-last [1 1 1 1] 0)


(defn next-i
  ([snails stack]
   (loop [r stack]
     (println "r" r "get:" (get-in snails r))
     (if (and (not= (get-in snails r) -1) (not (vector? (get-in snails r))))
       r
       (if (vector? (get-in snails r))
         (recur (conj r 0))
         (let [new-r (replace-last r 0)]
           (if (nil? new-r)
             nil
             ;;(recur new-r)
             new-r
             
             )))))))

(+ 1 2)


(next-i [[3 [2 [1 -1]]] [6 [5 [4 [3 2]]]]] [0 1 1 1])


(defn first-i
  ([snails]
   (loop [r [0]]
     (if (vector? (get-in snails r))
       (recur (conj r 0))
       r))))

(first-i [[[[[9 8] 1] 2] 3] 4])

(defn prev-i
  ([snails stack]
   (loop [r stack]
     (if (vector? (get-in snails r))
       (recur (conj r 1))
       (let [new-r (replace-last r 1)]
         (if (nil? new-r)
           nil
           new-r))))))


(defn update-snails [snails path]
  (let [r-path (pop path)
        ppath (peek path)
        c (get-in snails r-path)
        _ (println "old array" c)
        _ (println "ppath" ppath)
        new-snails (assoc-in snails r-path -1)
        _ (println "path to null" r-path)
        _ (println "old-snails" snails)
        _ (println "new-snails" new-snails)
        l (prev-i new-snails r-path)
        r (next-i new-snails r-path)
        _ (println "p" l "n" r (get-in snails r))
        new-snails (assoc-in snails r-path 0)
        new-snails (if (nil? l) new-snails (assoc-in new-snails l (+ (left c) (get-in new-snails l))))
        new-snails (if (nil? r) new-snails (assoc-in new-snails r (+ (right c) (get-in new-snails r))))]
    ;; (-> snails
    ;;     (assoc-in l (+ (left c) (get-in snails l 0))))
    new-snails))


(defn normalization [snails]
  (loop [stack (first-i snails)]
    (println "stack" stack)
    (if (> (count stack) 4)
      (update-snails snails stack)
      (let [new-stack (next-i snails stack)]
        (if (nil? new-stack)
          snails
          (recur new-stack))))))


(assert (= [[[[0,9],2],3],4] (normalization [[[[[9,8],1],2],3],4])))
(assert (= [7,[6,[5,[7,0]]]] (normalization [7,[6,[5,[4,[3,2]]]]])))
(assert (= [[6,[5,[7,0]]],3] (normalization [[6,[5,[4,[3,2]]]],1])))
(assert (= [[6,[5,[7,0]]],3] (normalization [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]])))

(defn part1 [input]
  (reduce #(normalization [%1 %2]) input))

(assert (= [[1,2],[[3,4],5]] (part1 `([1,2] [[3,4],5]))))


(assert (= 45 (time (part1 input-from-file-test))))
(assert (= 2850 (time (part1 input-from-file))))



(defn part2 [input]
  (->> input))

(assert (= 112 (time (part2 input-from-file-test))))
(assert (= 1117 (time (part2 input-from-file))))
