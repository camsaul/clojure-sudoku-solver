(ns solver
  (:use [clojure.set :as set]))

(def rows (range 0 81))
(def cols (apply interleave (partition 9 rows)))
(def squares (letfn  [(sq-index [i] [(quot (quot i 9) 3) (quot (rem i 9) 3)])]
               (sort-by sq-index rows)))
 
(defn reduce-set [set taken]
  (let [reduced (set/difference set taken)]
    (if (= 1 (count reduced)) (first reduced)
        reduced)))
 
(defn reduce-group [group]
  (let [taken (set (filter #(not (set? %)) group))]
   (map #(if (set? %) (reduce-set % taken) %) group)))
 
(defn reduce-by [group-type board]
  (let [reduced (->> (map board group-type)
                     (partition 9)
                     (mapcat reduce-group))]
    (mapv (vec reduced) group-type)))
 
(defn solve-board [board]
  (loop [board (mapv #(if (zero? %) (set (range 1 10)) %) board)]
    (let [reduced (->> board (reduce-by squares) (reduce-by rows) (reduce-by cols))]
      (if (not-any? set? reduced) (partition 9 reduced)
          (recur reduced)))))
