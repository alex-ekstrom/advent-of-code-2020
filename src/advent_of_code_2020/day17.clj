(ns advent-of-code-2020.day17
  (:gen-class)
  (:require [clojure.string :as str]
            [advent-of-code-2020.util :refer :all]))

(defn- ->coords [values x z w]
  (map-indexed #(if (= "#" %2) {[x %1 z 0] 1} {[x %1 z 0] 0}) values))

(def data 
  (str/split-lines (slurp "resources/day17.raw")))

(defn- parse [data]
  (->> (map #(str/split % #"") data)
       (map-indexed #(->coords %2 %1 0 0))
       (map #(reduce conj %))
       (reduce conj)))

(defn- get-neighbor-coords-4 [[x y z w]]
  (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1] dw [-1 0 1]
        :when (not (every? zero? [dz dy dx dw]))]
    [(+ x dx) (+ y dy) (+ z dz) (+ w dw)]))

(defn- get-neighbor-coords-3 [coords]
  (filter (fn [[_ _ _ w]] (zero? w)) (get-neighbor-coords-4 coords)))

(defn- get-neighbors [coord active? coords finder]
  (filter #(= active? (get coords % 0)) (finder coord)))

(defn- step [coords finder]
  (let [active (keys (filter-map [coord value] (= value 1) coords))
        deactive (distinct (filter #(not (<= 2 (count (get-neighbors % 1 coords finder)) 3)) active))
        inactive (distinct (reduce concat (map #(get-neighbors % 0 coords finder) active)))
        activate (distinct (filter #(= 3 (count (get-neighbors % 1 coords finder))) inactive))
        coords2 (reduce dissoc coords deactive)]
    (reduce #(assoc %1 %2 1) coords2 activate)))

(def test-data [".#." "..#" "###"])

(defn -main []
  (let [test-data (parse test-data)
        input-data (parse data)]
    (println (count (filter #(= 1 (second %)) (nth (iterate #(step % get-neighbor-coords-3) test-data) 6))))
    (println (count (filter #(= 1 (second %)) (nth (iterate #(step % get-neighbor-coords-3) input-data) 6))))
    (println (count (filter #(= 1 (second %)) (nth (iterate #(step % get-neighbor-coords-4) test-data) 6))))
    (println (count (filter #(= 1 (second %)) (nth (iterate #(step % get-neighbor-coords-4) input-data) 6))))))
