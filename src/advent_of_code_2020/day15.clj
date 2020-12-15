(ns advent-of-code-2020.day15
  (:gen-class)
  (:require [clojure.string :as str]
            [advent-of-code-2020.util :refer :all]))

(def data 
  (->> (str/split (str/trim-newline (slurp "resources/day15.raw")) #",")
       (map #(Long/parseLong %))))

(defn- step-one [[mem old] turn]
  (let [[prev pprev] (get mem old)]
    (if pprev
      [(update mem (- prev pprev) #(list turn (first %))) (- prev pprev)]
      [(update mem 0 #(list turn (first %))) 0])))

(defn- part-one [nums size]
  (let [starter (reduce conj (map-indexed #(hash-map %2 (list (inc %1))) nums))]
    (second (reduce step-one [starter (last nums)] (range (inc (count starter)) (inc size))))))

(defn -main []
  (println (part-one data 2020))
  (println (part-one data 30000000)))
