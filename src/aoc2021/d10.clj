(ns  aoc2021.d10
  (:require [clojure.string :as str]))



(def input-from-file-test
  (->> (slurp "resources/aoc2021/day10_t")
       (str/split-lines)))

(def input-from-file
  (->> (slurp "resources/aoc2021/day10_1")
       (str/split-lines)))

(defn check-brackets [brackets]
  (loop [b (vec brackets) s []]
    (if (empty? b)
      nil
      (let [f (first b)]
        (case f
          \( (recur (rest b) (conj s f))
          \[ (recur (rest b) (conj s f))
          \{ (recur (rest b) (conj s f))
          \< (recur (rest b) (conj s f))
          \) (if (= (last s) \() (recur (rest b) (pop s)) \))
          \] (if (= (last s) \[) (recur (rest b) (pop s)) \])
          \} (if (= (last s) \{) (recur (rest b) (pop s)) \})
          \> (if (= (last s) \<) (recur (rest b) (pop s)) \>)
          )))))


(assert (= \] (check-brackets "(]")))
(assert (= \] (check-brackets "()]")))
(assert (= \} (check-brackets "{([(<{}[<>[]}>{[]{[(<()>")))
(assert (= \) (check-brackets "[[<[([]))<([[{}[[()]]]")))
(assert (= \] (check-brackets "[{[{({}]{}}([{[{{{}}([]")))
(assert (= \) (check-brackets "[<(<(<(<{}))><([]([]()")))
(assert (= \> (check-brackets "<{([([[(<>()){}]>(<<{{")))



(defn part1 [input]
  (->> input
       (map check-brackets)
       (frequencies)
       (map (fn [[c n]] 
              (case c
                \) (* 3 n)
                \] (* 57 n)
                \} (* 1197 n)
                \> (* 25137 n)
                0)))
       (apply +)))

(assert (= 26397 (part1 input-from-file-test)))
(assert (= 339537 (part1 input-from-file)))


(defn part2 [input]
  (->> input
       ))

(assert (= 1 (part2 input-from-file-test)))
(assert (= 1 (part2 input-from-file)))
