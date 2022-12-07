(ns aoc2022.d7
  (:require 
    [clojure.string :as str]
    [clj-async-profiler.core :as prof]))


(defn parse-input [s] 
  (let [l (clojure.string/split s #"\n")
        c (first l)
        o (rest l)] [c o]))


(def test-input (clojure.string/split (slurp "resources/aoc2022/day7_t") #"\$ "))
(def input (clojure.string/split (slurp "resources/aoc2022/day7_1") #"\$ "))


(defn parse-file [f]
  (let [[_ size name] (re-matches #"(\d*) (.*)" f)]
    {:s (Long/parseLong size) :n name}))

(assert (= {:s 14848514, :n "b.txt"} (parse-file "14848514 b.txt")))

(defn parse-files [files] 
  (loop [f files r []] 
    (if (empty? f) 
      {:files r}
      (if (str/starts-with? (first f) "dir") 
        (recur (rest f) r)
        (recur (rest f) (conj r (parse-file (first f))))))))

(assert (= {:files [{:s 14848514, :n "b.txt"} {:s 8504156, :n "c.dat"}]} (parse-files `("dir a" "14848514 b.txt" "8504156 c.dat" "dir d"))))


  
(defn build-dirs [commands] 
  (loop [c commands r {} s []] 
    (if (empty? c) 
      r
      (let [[i o] (first c)]
        (cond
          (= i "cd ..") (recur (rest c) r (pop s))
          (str/starts-with? i "cd ") (recur (rest c) r (conj s (subs i 3)))
          (= i "ls") (recur (rest c) (assoc-in r s (parse-files o)) s)
          ;(assoc-in r (conj s (subs i 3)) {:dir true})
          :else (recur (rest c) r s))))))



(defn get-size-directory [d] (reduce + (map #(:s %) (:files d))))

(assert (= 30 (get-size-directory {:files [{:s 10} {:s 20}]})))
(assert (= 0 (get-size-directory {})))


(defn calc-directories
  ([d] (calc-directories d ["/"]))
  ([d s] 
   (let [new-d (get-in d s)
         children (map (fn [k] (calc-directories d (conj s k))) (keys (dissoc new-d :files)))
         size (get-size-directory new-d)]
     {:s (+ size (reduce + (map :s children))) :c (vec children)})))



(assert (= {:s 48381165, :c [{:s 94853, :c [{:s 584, :c []}]} {:s 24933642, :c []}]} 
          (calc-directories {"/"
                             {:files [{:s 14848514, :n "b.txt"} {:s 8504156, :n "c.dat"}],
                              "a" {:files [{:s 29116, :n "f"} {:s 2557, :n "g"} {:s 62596, :n "h.lst"}], "e" {:files [{:s 584, :n "i"}]}},
                              "d" {:files [{:s 4060174, :n "j"} {:s 8033020, :n "d.log"} {:s 5626152, :n "d.ext"} {:s 7214296, :n "k"}]}}})))

(defn calc-total [d] 
  (if (empty? d)
    0
    (let [s (:s d)
          s (if (> s 100000) 0 s)
          r (reduce + (map calc-total (:c d)))
          ] (+ s r))))

(assert (= 95437 (calc-total {:s 48381165, :c [{:s 94853, :c [{:s 584, :c []}]} {:s 24933642, :c []}]})))


(assert (= 95437 (->> test-input
                   (filter not-empty)
                   (map parse-input)
                   build-dirs
                   calc-directories
                   calc-total)))

(assert (= 1334506 (->> input
                     (filter not-empty)
                     (map parse-input)
                     build-dirs
                     calc-directories
                     calc-total)))




(defn sorted-used-space [d]  
  (if 
    (empty? d)
    [0]
    (let [s (:s d)
          r (mapcat sorted-used-space (:c d))] 
      (sort (conj r s)))))

(defn calc-enough-space [m] 
  (let [used (last m)
        total 70000000
        free (- total used)
        needed (- 30000000 free)]
    (->> m
      (filter #(> % needed))
      first)))


(assert (= 24933642 (->> test-input
                      (filter not-empty)
                      (map parse-input)
                      build-dirs
                      calc-directories
                      sorted-used-space
                      calc-enough-space)))

(assert (= 7421137 (->> input
                     (filter not-empty)
                     (map parse-input)
                     build-dirs
                     calc-directories
                     sorted-used-space
                     calc-enough-space)))


(comment 
  (do
    (vec (for [i (range 1000)] (optimized-find-marker input 4)))
    (println "start profiling")
    (prof/start)
    (optimized-find-marker input 4)
    (println (prof/stop))
    (println "end profiling"))
  )