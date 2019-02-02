(ns  hdoc.d43
  (:require
   [clojure.test :refer :all]
   [clojure.string :as str]))


; Given a singly linked list and an integer k, remove the kth last element from the list. k is guaranteed to be smaller than the length of the list.
; The list is very long, so making more than one pass is prohibitively expensive.
; Do this in constant space and in one pass.

(defprotocol Node-interface 
  (next [_])
  (value [_])
  (set [this n]))

(deftype Node [value ^{:volatile-mutable true} node]
  Node-interface
  (next [_] node)
  (value [_] value)
  (set [_ n] (set! node n)))


(defn find-last [node k]
  (loop [i 1
         node node
         result nil]
    (if (nil? node) result
        (recur (inc i) (.next node) (if (and (= (mod (inc i) k) 0) (not (nil? (.next node)))) node result)))))


;1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7
; k = 2 -> 6 - 1
; k = 4 -> 4 - 1 

(let [f (Node. 7 nil)
      f (Node. 6 f)
      f (Node. 5 f)
      f (Node. 4 f)
      f (Node. 3 f)
      f (Node. 2 f)
      f (Node. 1 f)]
    ; (loop [i 0])
  (assert (= 5 (.value (find-last f 2)))))

(let [f (Node. 7 nil)
      f (Node. 6 f)
      f (Node. 5 f)
      f (Node. 4 f)
      f (Node. 3 f)
      f (Node. 2 f)
      f (Node. 1 f)]
  (assert (= 3 (.value (find-last f 4)))))

(defn remove-next-node [node]
  (.set node (.next (.next node))))

(let [f (Node. 7 nil)
      f (Node. 6 f)
      f (Node. 5 f)
      f (Node. 4 f)
      f (Node. 3 f)
      f (Node. 2 f)
      f (Node. 1 f)]
    ; (loop [i 0])
  (remove-next-node  (find-last f 4))
  (loop [f f] 
    (if (nil? f) f
        (let [next (.next f)]
          (println (.value f))
          (recur next))))
  )

