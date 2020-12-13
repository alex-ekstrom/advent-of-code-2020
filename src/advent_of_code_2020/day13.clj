(ns advent-of-code-2020.day13
  (:gen-class)
  (:require [clojure.string :as str]
            [advent-of-code-2020.util :refer :all]))

(defn- load-resource-file []
  (let [[earliest ids] (->> (slurp "resources/day13.raw")
                            (str/split-lines))]
    [(Long/parseLong earliest) (->> (str/split ids #"," )
                                    (map-indexed #(vector %2 %1))
                                    (filter #(not (= "x" (first %))))
                                    (map #(vector (Long/parseLong (first %)) (second %))))]))

(defn- find-earlies-bus [earliest-time ids]
  (->> (map first ids)
       (map #(vector (- % (mod earliest-time %)) %))
       (sort)
       (first)
       (apply *)))

(defn- get-mmi [a b]
  (cond (zero? a) 0
        (zero? (mod b a)) 1
        :else (- b
                 (quot (* (get-mmi (mod b a) a)
                          b)
                       a))))

(defn- find-earliest-congruence [ids]
  (let [n (transduce (map first) * ids)
        x (transduce (map #(* (second %)
                              (quot n (first %))
                              (get-mmi (quot n (first %)) (first %))))
                     +
                     ids)]
    (mod (- n x) n)))

(defn -main []
  (let [[earliest-time ids] (load-resource-file)]
    (println (find-earlies-bus earliest-time ids))
    (println (find-earliest-congruence ids))))
