(ns  hdoc.d43
  (:require
   [clojure.test :refer :all]
   [clojure.string :as str]))


; Given a singly linked list and an integer k, remove the kth last element from the list. k is guaranteed to be smaller than the length of the list.
; The list is very long, so making more than one pass is prohibitively expensive.
; Do this in constant space and in one pass.

(defprotocol Node-interface 
  (next [_])
  (value [_]))

(defrecord Node [value ^Node node]
  Node-interface
  (next [_] node)
  (value [_] value))


(let [f (Node. 7 nil)
      f (Node. 6 f)
      f (Node. 5 f)
      f (Node. 4 f)
      f (Node. 3 f)
      f (Node. 2 f)
      f (Node. 1 f)]
    ; (loop [i 0])
  (.next f)
  )



(assert (= [5 4 6 6 6 4 4 4 5] (windowMax [5 3 4 1 6 2 2 4 3 1 5] 3)))

