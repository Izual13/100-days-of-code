(ns aoc2022.d5
  (:require [clojure.string :as str]))

(def test-input (clojure.string/split (slurp "resources/aoc2022/day5_t") #"\n\n"))
(def input (clojure.string/split (slurp "resources/aoc2022/day5_1") #"\n\n"))


(defn parse-moves [s]
  (let [[_ a b c] (re-matches #"move (\d*) from (\d*) to (\d*)" s)]
    [(Integer/parseInt a) (Integer/parseInt b) (Integer/parseInt c)]))

(assert (= [28 23 17] (parse-moves "move 28 from 23 to 17")))

(defn parse-letter [s]
  (let [[_ a] (re-matches #"\[(.)\]\s?" s)]
    a))

(defn parse-stack [s] 
  (loop [s s r []] 
    (if (empty? s) 
      r
      (let [right (min 4 (count s))
            f (parse-letter (subs s 0 right))]
        (recur (subs s right) (conj r f))))))

(assert (= [nil "D" nil] (parse-stack "    [D]    ")))
(assert (= ["N" "C" nil] (parse-stack "[N] [C]    ")))
(assert (= ["Z" "M" "P"] (parse-stack "[Z] [M] [P]")))


(defn parse-input [[s m]] 
  (let [l (str/split-lines s)
        c (/ (+ 1 (count (first l))) 4)
        stacks (loop [l l r (vec (for [x (range c)] []))]
                 (if (empty? l) 
                   r
                   (recur (rest l) 
                     (->> l
                       first
                       parse-stack
                       (map-indexed #(if (nil? %2) (get r %1) (conj (get r %1) %2)))
                       vec))))]
    
    [stacks (map parse-moves (str/split-lines m))]))

(assert (= [[["N" "Z"] ["D" "C" "M"] ["P"]] `([1 2 1] [3 1 3] [2 2 1] [1 1 2])] (parse-input test-input)))


(defn rearrangement [[stacks moves]] 
  (loop [m moves s stacks] 
    (if (empty? m)
      s
      (let [[c from to] (first m)]
        (recur (rest m) 
          (loop [i 0 s s] 
            (if (= i c) 
              s
              (let [e (first (get s (- from 1)))]
                (if (nil? e)
                  s
                  (recur (inc i) 
                    (-> s 
                      (update (- from 1) rest)
                      (update  (- to 1) #(cons e %)))))))))))))

(assert (= "CMZ" 
          (->> test-input
            parse-input
            rearrangement
            (map first)
            (apply str))))

(assert (= "SBPQRSCDF" 
          (->> input
            parse-input
            rearrangement
            (map first)
            (apply str))))


(defn rearrangement-2 [[stacks moves]] 
  (loop [m moves s stacks] 
    (if (empty? m)
      s
      (let [[c from to] (first m)
            e (take c (get s (- from 1)))]
        (recur (rest m) 
          (-> s
            (update (- from 1) #(nthrest % c))
            (update (- to 1) #(reduce (fn [element array] (cons array element)) % (reverse e)))))))))

(assert (= ["D" "N" "Z" "P"] (reduce #(cons %2 %1) ["Z" "P"] (reverse ["D" "N"]))))

(assert (= "MCD" 
          (->> test-input
            parse-input
            rearrangement-2
            (map first)
            (apply str))))

(assert (= "RGLVRCQSB" 
          (->> input
            parse-input
            rearrangement-2
            (map first)
            (apply str)
            )))