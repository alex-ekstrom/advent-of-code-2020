(ns advent-of-code-2020.day09
  (:gen-class)
  (:require [clojure.string :as str]
            [advent-of-code-2020.util :refer :all]))

(defn- load-resource-file []
  (->> (str/split (slurp "resources/day09.raw") #"\n")
       (map #(Long/parseLong %))
       (vec)))

(defn- ->added-sum [value el]
  (let [[num sums] el]
    [num (conj sums (+ num value))]))

(defn- add-sums [col value]
  (-> (map #(->added-sum value %) col)
      (vec)
      (conj [value []])))

(defn- is-valid? [sums val]
  (->> (map second sums)
       (flatten)
       (filter #(= val %))
       (not-empty)))

(defn- slide-window [inputs sums]
  (let [[value & remain] inputs]
    (if (is-valid? sums value)
      (recur remain (add-sums (rest sums) value))
      value)))

(defn- find-invalid [inputs preamble-size]
  (slide-window (subvec inputs preamble-size)
                (reduce add-sums [] (subvec inputs 0 preamble-size))))

(defn- get-values-lt 
  ([inputs value]
   (get-values-lt inputs value ()))
  ([inputs value current]
   (let [[val & remain] inputs
         new (conj current val)
         sum (reduce + current)]
     (if (<= value sum)
       (conj new sum)
       (recur remain value new)))))

(defn- find-contiguous [inputs value]
  (let [[sum & range] (get-values-lt inputs value)]
    (if (= sum value)
      (+ (apply min range) (apply max range))
      (recur (rest inputs) value))))

(defn -main []
  (let [file-values (load-resource-file)]
    (println (find-invalid file-values 25))
    (println (find-contiguous file-values 23278925))))
