(ns advent-of-code-2020.day06
  (:gen-class)
  (:require [clojure.string :as str]
            [advent-of-code-2020.util :refer :all]))

(defn- load-resource-file []
  (str/split (slurp "resources/day06.raw") #"\n\n"))

(defn- count-unique [input]
  (count (keys (dissoc (frequencies input) \newline))))

(defn- count-max [input]
  (let [responses (frequencies input)]
    (->> (dissoc responses \newline)
         (vals)
         (filter #(= % (inc (get responses \newline 0))))
         (count))))

(defn- count-questions [inputs method]
  (->> inputs
       (map method)
       (reduce +)))

(defn -main []
  (let [file-values (load-resource-file)]
    (println (count-questions file-values count-unique))
    (println (count-questions file-values count-max))))
