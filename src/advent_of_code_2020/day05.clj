(ns advent-of-code-2020.day05
  (:gen-class)
  (:require [clojure.string :as str]
            [advent-of-code-2020.util :refer :all]))

(defn- load-resource-file []
  (->> (str/split (slurp "resources/day05.raw") #"\n")
       (map #(str/replace % #"F" "0"))
       (map #(str/replace % #"B" "1"))
       (map #(str/replace % #"L" "0"))
       (map #(str/replace % #"R" "1"))))

(defn- get-max [inputs]
  (->> inputs
       (map #(+ (* (Integer/parseInt (subs % 0 7) 2) 8) (Integer/parseInt (subs % 7) 2)))
       (apply max)))

(defn- get-missing [inputs]
  (let [all-seats (->> inputs
                    (map #(+ (* (Integer/parseInt (subs % 0 7) 2) 8) (Integer/parseInt (subs % 7) 2)))
                    (apply hash-set))]
    (->> (range 0 861) 
         (filter #(and (not (contains? all-seats %)) (contains? all-seats (dec %)) (contains? all-seats (inc %))))
         (first))))

(defn -main []
  (let [file-values (load-resource-file)]
    (println (get-max file-values))
    (println (get-missing file-values))))
