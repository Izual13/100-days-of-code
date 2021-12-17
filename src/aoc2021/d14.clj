(ns  aoc2021.d14
  (:require [clojure.string :as str]))



(def input-from-file-test
  (let [[polymer pairs] (str/split (slurp "resources/aoc2021/day14_t") #"\r?\n\r?\n")
        parsed-pairs (->> pairs
                          (str/split-lines)
                          (mapv #(let [[_ a b c] (re-matches #"(\w)(\w) -> (\w)" %)] {(str a b) [(str a c) (str c b)]}))
                          (into {}))]
    {:polymer polymer :pairs parsed-pairs}))


(into {} [["1" "2"] ["3" "4"]])
(flatten [ [{1 2} {3 4}] [{1 2} {3 4}]])

(assert (= ["CH" "B"] (let [[_ c p] (re-matches #"(\w+) -> (\w+)" "CH -> B")] [c p])))

(merge {:a 1 :b 2 :c 3} {:b 9 :d 4} {:z 0 :x 4})

(def input-from-file
  (let [[paper insrruction] (str/split (slurp "resources/aoc2021/day13_1") #"\r?\n\r?\n")
        parsed-paper (->> paper
                          (str/split-lines)
                          (mapv #(str/split % #","))
                          (mapv #(mapv (fn [x] (Long/parseLong x)) %)))
        parsed-insrructions (->> insrruction
                                 (str/split-lines)
                                 (mapv #(let [[_ c p] (re-matches #"fold along (\w+)=(\d+)" %)] [c (Long/parseLong p)])))]
    {:points parsed-paper :insrructions parsed-insrructions}))

(defn build-map [polymer]
  (println "polymer" polymer)
  (->>
   (for [i (range 1 (count polymer))] {(str (get polymer (dec i)) (get polymer i)) 1})
   (into {})))


(defn part1 [input]
  (let [m (build-map (:polymer input))] m))



(assert (= 17 (part1 input-from-file-test)))
(assert (= 647 (part1 input-from-file)))


(defn part2 [input]
  (let []))

(assert (= 16 (time (part2 input-from-file-test))))
(assert (= 93 (time (part2 input-from-file))))


