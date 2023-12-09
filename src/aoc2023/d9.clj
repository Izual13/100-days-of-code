(ns  aoc2023.d9
  (:require [clojure.string :as str]))

(def history (clojure.string/split (slurp "resources/aoc2023/day9_1") #"\r?\n"))

(def history-t (clojure.string/split (slurp "resources/aoc2023/day9_t") #"\r?\n"))

(defn parse-array [s] 
  (let [r (clojure.string/split s #" +")]
    (mapv #(Integer/parseInt %1) r)))


(->> history-t
  (map parse-array))