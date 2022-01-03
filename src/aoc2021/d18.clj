(ns  aoc2021.d18
  (:require [clojure.string :as str]
            [clj-async-profiler.core :as prof]))


(def input-from-file-test
  (->> (slurp "resources/aoc2021/day18_t")
       (str/split-lines)
       (map read-string)
       (vec)))

(def input-from-file-test2
  (->> (slurp "resources/aoc2021/day18_t2")
       (str/split-lines)
       (map read-string)
       (vec)))


(def input-from-file
  (->> (slurp "resources/aoc2021/day18_1")
       (str/split-lines)
       (map read-string)
       (vec)))

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

(assert (nil? (replace-last [1 1 1 1] 0)))
(assert (= [1 1 1 1] (replace-last [1 1 1 0] 0)))

(defn next-i
  ([snails stack]
   (let [r (replace-last stack 0)]
     (if (nil? r)
       nil
       (loop [r r]
         (if (vector? (get-in snails r))
           (recur (conj r 0))
           r))))))


(assert (= [1 0] (next-i [[3 [2 [1 -1]]] [6 [5 [4 [3 2]]]]] [0 1 1 1])))
(assert (= [1 0] (next-i [7,[6,[5,[4,[3,2]]]]] [0])))
(assert (= [0 0 1] (next-i [[[[0 9] 2] 3] 4] [0 0 0 1])))

(defn prev-i
  ([snails stack]
   (let [r (replace-last stack 1)]
     (if (nil? r)
       nil
       (loop [r r]
         (if (vector? (get-in snails r))
           (recur (conj r 1))
           r))))))


(assert (nil? (prev-i [7,[6,[5,[4,[3,2]]]]] [0])))
(assert (= [0] (prev-i [7,[6,[5,[4,[3,2]]]]] [1 0])))

(defn first-i
  ([snails]
   (loop [r [0]]
     (if (vector? (get-in snails r))
       (recur (conj r 0))
       r))))

(assert (= [0] (first-i [7,[6,[5,[4,[3,2]]]]])))


(defn explode-snails [snails path]
  (let [r-path (pop path)
        c (get-in snails r-path)
        new-snails (assoc-in snails r-path 0)
        l (prev-i new-snails r-path)
        r (next-i new-snails r-path)
        new-snails (if (nil? l) new-snails (assoc-in new-snails l (+ (left c) (get-in new-snails l))))
        new-snails (if (nil? r) new-snails (assoc-in new-snails r (+ (right c) (get-in new-snails r))))]
    new-snails))

(defn split-snails [s]
  (loop [i (first-i s)]
    (if (nil? i) [s false]
        (let [v (get-in s i)
              ni (next-i s i)]
          (if (> v 9)
            [(assoc-in s i [(int (/ v 2)) (int (Math/ceil (/ v 2)))]) true]
            (recur ni))))))


(assert (= 2 (int (/ 5 2))))
(assert (= 3 (int (Math/ceil (/ 5 2)))))
(assert (= [[[[0 7] 4] [[7 8] [0 13]]] [1 1]] (left (split-snails [[[[0,7],4],[15,[0,13]]],[1,1]]))))
(assert (= [[[[0 7] 4] [[7 8] [0 [6 7]]]] [1 1]] (left (split-snails [[[[0 7] 4] [[7 8] [0 13]]] [1 1]]))))

(defn normalization [snails]
  (loop [stack (first-i snails) result snails]
    (if (nil? stack)
      (let [[new-result changed] (split-snails result)]
        (if (false? changed)
          result
          (recur (first-i new-result) new-result)))
      (if (> (count stack) 4)
        (let [new-result  (explode-snails result stack)]
          (recur (next-i new-result (pop stack)) new-result))
        (recur (next-i result stack) result)))))


(assert (= [[[[0,9],2],3],4] (normalization [[[[[9,8],1],2],3],4])))
(assert (= [7,[6,[5,[7,0]]]] (normalization [7,[6,[5,[4,[3,2]]]]])))
(assert (= [[6,[5,[7,0]]],3] (normalization [[6,[5,[4,[3,2]]]],1])))
(assert (= [[3,[2,[8,0]]],[9,[5,[7,0]]]] (normalization [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]])))
(assert (= [[[[0,7],4],[[7,8],[6,0]]],[8,1]] (normalization [[[[[4,3],4],4],[7,[[8,4],9]]] [1,1]])))



(defn calc
  ([a] (if (vector? a) (calc (left a) (right a)) a))
  ([a b] (+ (* 3 (calc a)) (* 2 (calc b)))))

(assert (= 29 (calc [9,1])))
(assert (= 129 (calc [[9,1],[1,9]])))
(assert (= 3488 (calc [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]])))

(defn part1 [input]
  (->> input
       (reduce #(normalization [%1 %2]))
       (calc)))

(assert (= 143 (time (part1 [[1,2],[[3,4],5]]))))
(assert (= 1384 (time (part1 [[[[0,7],4],[[7,8],[6,0]]],[8,1]]))))
(assert (= 445 (time (part1 [[[[1,1],[2,2]],[3,3]],[4,4]]))))
(assert (= 791 (time (part1 [[[[3,0],[5,3]],[4,4]],[5,5]]))))
(assert (= 1137 (time (part1 [[[[5,0],[7,4]],[5,5]],[6,6]]))))
(assert (= 3488 (time (part1 [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]))))

(assert (= 3488 (time (part1 input-from-file-test))))
(assert (= 3486 (time (part1 input-from-file))))



(defn part2 [input]
  (apply max (for [i (range (count input))
                   j (range (count input))
                   :when (not= i j)]
               (->> (normalization [(get input i) (get input j)])
                    (calc)))))

(assert (= 3993 (time (part2 input-from-file-test2))))
(assert (= 4747 (time (part2 input-from-file))))



(do
  (println "start profiling")
  (prof/start)
  (part2 input-from-file)
  (println (prof/stop))
  (println "end profiling"))