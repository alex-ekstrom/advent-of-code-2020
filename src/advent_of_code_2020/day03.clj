(ns advent-of-code-2020.day03
  (:gen-class)
  (:require [clojure.string :as str]
            [advent-of-code-2020.util :refer :all]))

(defn- load-resource-file []
  (->> (str/split (slurp "resources/day03.raw") #"\n")))

(defn- get-trees-hit [inputs rslope dslope]
  (->> inputs
       (keep-indexed #(if (= 0 (mod %1 dslope)) %2))
       (keep-indexed #(if (= \# (nth %2 (mod (* %1 rslope) (count %2)))) %2))
       (count)))

(defn -main []
  (let [file-values (load-resource-file)]
    (println (get-trees-hit file-values 3 1))
    (println (* (get-trees-hit file-values 1 1)
                (get-trees-hit file-values 3 1)
                (get-trees-hit file-values 5 1)
                (get-trees-hit file-values 7 1)
                (get-trees-hit file-values 1 2)))))
