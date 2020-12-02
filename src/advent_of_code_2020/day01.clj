(ns advent-of-code-2020.day01
  (:gen-class)
  (:require [clojure.string :as str]))

(defn- load-resource-file []
  (->> (str/split (slurp "resources/day01.raw") #"\n")
       (map #(Integer/parseInt %))))

(defn- find-expense-pair
  ([expenses]
   (find-expense-pair expenses #{}))
  ([expenses checks]
   (let [val (first expenses)
         inv (- 2020 val)]
     (if (contains? checks val)
       (* val inv)
       (recur (rest expenses) (conj checks inv))))))

(defn- find-expense-trio
  ([expenses]
   (find-expense-trio expenses [] {}))
  ([expenses prevs checks]
   (let [val (first expenses)]
     (if (contains? checks val)
       (* val (first (get checks val)) (second (get checks val)))
       (recur (rest expenses) 
              (conj prevs val)
              (into checks
                    (map #(hash-map (- 2020 % val) [% val]) prevs)))))))

(defn -main []
  (let [file-values (load-resource-file)]
    (println (find-expense-pair file-values))
    (println (find-expense-trio file-values))))
