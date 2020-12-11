(ns advent-of-code-2020.day11
  (:gen-class)
  (:require [clojure.string :as str]
            [advent-of-code-2020.util :refer :all]))

(def adjacency-dirs [[-1 -1] [-1 0] [-1 1]
                     [0 -1] [0 1]
                     [1 -1] [1 0] [1 1]])

(defn- load-resource-file []
  (->> (slurp "resources/day11.raw")
       (str/split-lines)
       (map vec)
       (vec)))

(defn- find-next-seat [x y dir inputs max-dist]
  (loop [dist max-dist x x y y seat \.]
    (let [new-x (+ x (first dir))
          new-y (+ y (second dir))]
      (if (or (not (< -1 new-x (count inputs)))
              (not (< -1 new-y (count (first inputs))))
              (= 0 dist))
        seat
        (let [new-seat (get-in inputs [new-x new-y])]
          (if-not (= \. new-seat)
            new-seat
            (recur (dec dist) new-x new-y new-seat)))))))

(defn- get-surrounding [x y inputs max-dist]
  (map #(find-next-seat x y % inputs max-dist) adjacency-dirs)) 

(defn- get-next-phase [inputs max-dist adj-seats]
  (->> (partition (count (first inputs))
             (for [[i row]  (map-indexed list inputs)
                   [j cell] (map-indexed list row)]
               (case cell
                     \. \.
                     \L (if (= 0 (get (frequencies (get-surrounding i j inputs max-dist)) \# 0))
                          \#
                          \L)
                     \# (if (<= adj-seats (get (frequencies (get-surrounding i j inputs max-dist)) \# 0))
                          \L
                          \#))))
       (map vec)
       (vec)))

(defn- find-stale [current max-dist adj-seats]
  (loop [state current times 0]
    (let [next (get-next-phase state max-dist adj-seats)]
      (if (= state next)
        (get (frequencies (flatten state)) \#)
        (recur next (inc times))))))

(defn -main []
  (let [file-values (load-resource-file)]
    (println (find-stale file-values 1 4))
    (println (find-stale file-values 100 5))))
