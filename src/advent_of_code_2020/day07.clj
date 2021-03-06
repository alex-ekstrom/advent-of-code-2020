(ns advent-of-code-2020.day07
  (:gen-class)
  (:require [clojure.string :as str]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]
            [advent-of-code-2020.util :refer :all]))

(defn- ->edges [line]
  (let [primary   (second (re-find (re-matcher #"(.*?) bags" line)))
        secondary (re-seq #"(\d) ([a-z\s]*) bags?" line)]
    (if (some? secondary)
      (map #(vector (nth % 2) primary {:amount (Integer/parseInt (second %))}) secondary)
      [primary])))

(defn- load-resource-file []
  (->> (str/split (slurp "resources/day07.raw") #"\n")
       (map ->edges)
       (apply concat)
       (apply uber/digraph)))

(defn- get-span-count [graph start]
  (-> (alg/bf-traverse graph start)
      (count)
      (dec)))

(defn- count-bags [graph start]
  (->> (uber/out-edges graph start)
       (map #(+ (uber/attr graph % :amount) (* (uber/attr graph % :amount) (count-bags graph (uber/dest %)))))
       (reduce +)))

(defn- get-sub-total [graph start]
  (-> (uber/transpose graph)
      (count-bags start)))

(defn -main []
  (let [file-values (load-resource-file)]
    (println (get-span-count file-values "shiny gold"))
    (println (get-sub-total file-values "shiny gold"))))
