(ns advent-of-code-2020.day14
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [advent-of-code-2020.util :refer :all]))

(defn- parse-instruction [line]
  (if (re-matches #"^mask.*" line)
    (let [[_ value] (re-find (re-matcher #"= (.*)" line))]
      {:inst :mask
       :x (str/replace value #"\d" "0")
       :mask (Long/parseLong (str/replace value #"X" "0") 2) 
       :inv (Long/parseLong (str/replace (str/replace value #"\d" "0") #"X" "1") 2)
       :dinv (Long/parseLong (str/replace (str/replace value #"\d" "1") #"X" "0") 2)})
    (let [[_ address value] (re-find (re-matcher #"mem\[(\d+)\] = (.*)" line))]
      {:inst :set 
       :loc (Long/parseLong address) 
       :val (Long/parseLong value)})))

(defn- load-resource-file []
  (->> (slurp "resources/day14.raw")
       (str/split-lines)
       (map parse-instruction)))

(defn- apply-value-mask [value mask inv]
  (bit-or mask (bit-and value inv)))

(defn- run-instruct-val [[mem mask inv] inst]
  (case (:inst inst)
    :mask [mem (:mask inst) (:inv inst)]
    :set [(assoc mem (:loc inst) (apply-value-mask (:val inst) mask inv)) mask inv]))

(defn- run-program-val [instructions]
  (apply + (vals (first (reduce run-instruct-val [{} 0 0 0 ""] instructions)))))

(defn- apply-mem-mask [mem loc value mask inv floating]
  (let [mval (bit-or loc mask)
        xs (get (frequencies floating) \X)
        addrs   (for [total (range (inc xs))
                      perm (combo/permutations (concat (take total (repeat "1")) (take (- xs total) (repeat "0"))))]
                  (apply-value-mask mval (Long/parseLong (reduce #(str/replace-first %1 "X" %2) floating perm) 2) inv))]
    (transduce (map #(hash-map % value)) conj mem addrs)))

(defn- run-instruct-mem [[mem mask inv x] inst]
  (case (:inst inst)
    :mask [mem (:mask inst) (:dinv inst) (:x inst)]
    :set [(apply-mem-mask mem (:loc inst) (:val inst) mask inv x) mask inv x]))

(defn- run-program-mem [instructions]
  (apply + (vals (first (reduce run-instruct-mem [{} 0 0 0 ""] instructions)))))

(defn -main []
  (let [file-values (load-resource-file)]
    (println (run-program-val file-values))
    (println (run-program-mem file-values))))
