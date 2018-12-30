(ns  hdoc.d13
  (:require
   [clojure.string :as str]))

;Designer PDF Viewer

(def abc (into {} (map-indexed (fn [x y] [y x]) "abcdefghijklmnopqrstuvwxyz")))

(defn designerPdfViewer [h word]
  (def alphabet-map (map-indexed (fn [idx itm] (nth h (get abc itm))) word))
  (* (apply max alphabet-map) (.length word)))

(designerPdfViewer [1 3 1 3 1 4 1 3 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 7] "zaba")

(assert (= 9 (designerPdfViewer [1 3 1 3 1 4 1 3 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5] "abc")))
(assert (= 28 (designerPdfViewer [1 3 1 3 1 4 1 3 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 7] "zaba")))