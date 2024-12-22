(ns utils
  (:require [clojure.string :as str]
            [clj-async-profiler.core :as prof]))


(defn vec-insert [v i e] (vec (concat (take i v) [e] (drop i v))))
(defn vec-insertm [v i e] (vec (concat (take i v) e (drop i v))))
(defn vec-remove [v i] (into (subvec v 0 i) (subvec v (inc i))))

(vec-insert [1 2 3] 3 4)