(ns advent-of-code-2020.day10
  (:gen-class)
  (:require [clojure.string :as str]
            [advent-of-code-2020.util :refer :all]))

(defn- load-resource-file []
  (let [file-nums (->> (slurp "resources/day10.raw")
                       (str/split-lines)
                       (map #(Long/parseLong %))
                       (apply hash-set))]
    (conj file-nums 0 (+ 3 (apply max file-nums)))))

(defn- get-added-path [nums]
  (let [nodes (sort nums)
        diffs (->> (map #(- %1 %2) (rest nodes) nodes)
                   (frequencies))]
    (* (get diffs 1) (get diffs 3))))

(declare get-all-paths-memo)
(defn- get-all-paths [nums start]
  (if-let [children (not-empty (->> (range (inc start) (+ 4 start))
                                    (filter #(contains? nums %))))]
      (transduce (map #(get-all-paths-memo nums %)) + children)
      1))

(def get-all-paths-memo (memoize get-all-paths))

(defn -main []
  (let [file-values (load-resource-file)]
    (println (get-added-path file-values))
    (println (get-all-paths file-values 0))))
