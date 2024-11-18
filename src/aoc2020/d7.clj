(ns  aoc2020.d7
  (:require [clojure.string :as str]))


(def test-luggage (str/split (slurp "resources/aoc2020/day7_t") #"\n"))
(def test2-luggage (str/split (slurp "resources/aoc2020/day7_t2") #"\n"))
(def luggage (str/split (slurp "resources/aoc2020/day7_1") #"\n"))


(defn parse-luggage [l] 
  (let [[_ k vs] (re-matches #"(.*) bags contain (.*)" l)
        vs (mapv #(let [[_ n v] (re-matches #"(\d) (.*) bag.*" %)] (if (nil? v) v [(Integer/parseInt n) v])) (str/split vs #", "))
        vs (filterv #(not (nil? %)) vs)
        ]
    {k (set vs)}))

(defn count-colors 
  ([luggage-map] (for [k (keys luggage-map)] (count-colors luggage-map k)))
  ([m k] (loop [s #{k} v #{}] 
           (if (empty? s) 
             false
             (let [f (first s)]
               (cond (= "shiny gold" f) true
                 (contains? v f) (recur (disj s f) (conj v f))
                 :else (let [inside (mapv second (m f))]
                         (recur (apply conj (disj s f) inside) (conj v f)))))))))


(defn count-colors [luggage-map]
  (keys luggage-map))



(assert (= 4 (->> test-luggage
  (mapv parse-luggage)
  (reduce merge)
  (count-colors)
  (filter #(= true %))
  count
  dec)))


(assert (= 235 (->> luggage
  (mapv parse-luggage)
  (reduce merge)
  (count-colors)
  (filter #(= true %))
  count
  dec)))


(defn count-bags [k v m] 
  (loop [s #{k} r 1]
    (if (empty? s) 
      0
      (let [f (first s)]
        (cond 
          (contains? v f) (v f)
          (empty? (m f)) 0
          :else (+ (apply + (map first (m f))) (apply + (for [i (m f)] (* (first i) (count-bags (second i) v m)))))
          ; :else (count (m f))
        )))))
      
(assert (= 126 (->> test2-luggage
  (mapv parse-luggage)
  (reduce merge)
  (count-bags "shiny gold" {}))))

(assert (= 32 (->> test-luggage
  (mapv parse-luggage)
  (reduce merge)
  (count-bags "shiny gold" {}))))

(assert (= 158493 (->> luggage
  (mapv parse-luggage)
  (reduce merge)
  (count-bags "shiny gold" {}))))
