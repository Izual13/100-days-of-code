(ns  hdoc.d19
  (:require
   [clojure.string :as str]))

;Append and Delete

(defn appendAndDelete [s t k]
  (let [input (to-array t)
        output (to-array s)
        last-mathes (loop [i 0] (if (and (not (= i (count input))) (not (= i (count output))) (= (aget input i) (aget output i))) (recur (inc i)) i))
        result (+ (- (count input) last-mathes) (- (count output) last-mathes))
        result (if-not (= k result) (if (> k (+ (count output) (count input))) k result) result)
        result (if-not (= k result) (if (and (> k result) (even? (- k result)) (not (= 0 last-mathes))) k result) result)]
    (if (= k result) "Yes" "No")))



(assert (= "Yes" (appendAndDelete "hackerhappy" "hackerrank" 9)))
(assert (= "Yes" (appendAndDelete "aba" "aba" 7)))
(assert (= "Yes" (appendAndDelete "zzzzz" "zzzzzzz" 4)))
(assert (= "Yes" (appendAndDelete "zzzzz" "zzzzzzz" 2)))
(assert (= "No" (appendAndDelete "ashley" "ash" 2)))
(assert (= "No" (appendAndDelete "y" "yu" 2)))

