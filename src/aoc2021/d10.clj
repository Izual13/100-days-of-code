(ns  aoc2021.d10
  (:require [clojure.string :as str]))



(def input-from-file-test
  (->> (slurp "resources/aoc2021/day10_t")
       (str/split-lines)))

(def input-from-file
  (->> (slurp "resources/aoc2021/day10_1")
       (str/split-lines)))

(defn get-first-invalid-bracket [brackets]
  (loop [b (vec brackets) s []]
    (if (empty? b)
      nil
      (let [f (first b) r (rest b)]
        (case f
          \( (recur r (conj s f))
          \[ (recur r (conj s f))
          \{ (recur r (conj s f))
          \< (recur r (conj s f))
          \) (if (= (last s) \() (recur r (pop s)) \))
          \] (if (= (last s) \[) (recur r (pop s)) \])
          \} (if (= (last s) \{) (recur r (pop s)) \})
          \> (if (= (last s) \<) (recur r (pop s)) \>))))))

(assert (= \] (get-first-invalid-bracket "(]")))
(assert (= \] (get-first-invalid-bracket "()]")))
(assert (= \} (get-first-invalid-bracket "{([(<{}[<>[]}>{[]{[(<()>")))
(assert (= \) (get-first-invalid-bracket "[[<[([]))<([[{}[[()]]]")))
(assert (= \] (get-first-invalid-bracket "[{[{({}]{}}([{[{{{}}([]")))
(assert (= \) (get-first-invalid-bracket "[<(<(<(<{}))><([]([]()")))
(assert (= \> (get-first-invalid-bracket "<{([([[(<>()){}]>(<<{{")))

(defn inverse-bracket [b]   (case b
                              \( \)
                              \[ \]
                              \{ \}
                              \< \>))


(defn get-ending-brackets [brackets]
  (loop [b (vec brackets) s []]
    (if (empty? b)
      (if (empty? s) nil (reverse (map inverse-bracket s)))
      (let [f (first b) r (rest b)]
        (case f
          \( (recur r (conj s f))
          \[ (recur r (conj s f))
          \{ (recur r (conj s f))
          \< (recur r (conj s f))
          \) (if (= (last s) \() (recur r (pop s)) nil)
          \] (if (= (last s) \[) (recur r (pop s)) nil)
          \} (if (= (last s) \{) (recur r (pop s)) nil)
          \> (if (= (last s) \<) (recur r (pop s)) nil)
          f)))))


(assert (= [\} \} \] \] \) \} \) \]] (get-ending-brackets "[({(<(())[]>[[{[]{<()<>>")))

(defn calc-scores [brackes]
  (reduce (fn [a k] (+ (* a 5)
                       (case k
                         \) 1
                         \] 2
                         \} 3
                         \> 4
                         0))) 0 brackes))

(assert (= 288957 (calc-scores (vec "}}]])})]"))))


(defn part1 [input]
  (->> input
       (map get-first-invalid-bracket)
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
  (let [scores (->> input
                    (map get-ending-brackets)
                    (map calc-scores)
                    (sort)
                    (filterv #(not= 0 %)))
        _ (println scores)
        half-count (int (/ (count scores) 2))
        _ (println half-count)
        ] 
    (get scores half-count)))


(assert (= 288957 (part2 input-from-file-test)))
(assert (= 2412013412 (part2 input-from-file)))