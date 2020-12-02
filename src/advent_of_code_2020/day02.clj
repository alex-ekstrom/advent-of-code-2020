(ns advent-of-code-2020.day02
  (:gen-class)
  (:require [clojure.string :as str]
            [advent-of-code-2020.util :refer :all]))

(defn- load-resource-file []
  (->> (str/split (slurp "resources/day02.raw") #"\n")
       (map #(re-find (re-matcher #"(\d+)-(\d+) ([a-z]): ([a-z]+)" %)))
       (map #(vector (Integer/parseInt (nth % 1)) (Integer/parseInt (nth % 2)) (nth (nth % 3) 0) (nth % 4)))))

(defn- get-valid-count-passwords [inputs start]
  (if (= 0 (count inputs))
    start
    (let [[min max letter pass]  (first inputs)
          total                  (get (frequencies pass) letter 0)]
      (if (<= min total max)
       (recur (rest inputs) (inc start))
       (recur (rest inputs) start))))) 

(defn- get-valid-pos-passwords [inputs start]
  (if (= 0 (count inputs))
    start
    (let [[first second letter pass]  (first inputs)
          first  (dec first)
          second (dec second)]
      (if (xor (= (nth pass first) letter) (= (nth pass second) letter))
       (recur (rest inputs) (inc start))
       (recur (rest inputs) start)))))

(defn -main []
  (let [file-values (load-resource-file)]
    (println (get-valid-count-passwords file-values 0))
    (println (get-valid-pos-passwords file-values 0))))
