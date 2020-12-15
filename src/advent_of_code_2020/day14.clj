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
      (prn address (Long/Long/toBinaryString (Long/parseLong address)))
      {:inst :set 
       :loc (Long/parseLong address) 
       :val (Long/parseLong value)})))

(defn- load-resource-file []
  (->> (slurp "resources/day14.raw")
       (str/split-lines)
       (map parse-instruction)))

(defn- apply-value-mask [value mask inv]
  (bit-or mask (bit-and value inv)))

(defn- run-program-val [instructions]
  (loop [instruction instructions mem {} mask 0 inv 0]
    (if-let [[inst & rest] instruction]
      (case (:inst inst)
        :mask (recur rest mem (:mask inst) (:inv inst))
        :set (recur rest (assoc mem (:loc inst) (apply-value-mask (:val inst) mask inv)) mask inv))
      (transduce (map val) + mem))))

(defn- apply-mem-mask [mem loc value mask inv floating]
  (let [mval (bit-or loc mask)
        xs (get (frequencies floating) \X)
        addrs   (for [total (range (inc xs))
                      perm (combo/permutations (concat (take total (repeat "1")) (take (- xs total) (repeat "0"))))]
                  (apply-value-mask mval (Long/parseLong (reduce #(str/replace-first %1 "X" %2) floating perm) 2) inv))]
    (transduce (map #(hash-map % value)) conj mem addrs)))

(defn- run-program-mem [instructions]
  (loop [instruction instructions mem {} x "" mask 0 inv 0]
    (if-let [[inst & rest] instruction]
      (case (:inst inst)
        :mask (recur rest mem (:x inst) (:mask inst) (:dinv inst))
        :set (recur rest (apply-mem-mask mem (:loc inst) (:val inst) mask inv x) x mask inv))
      (transduce (map val) + mem))))

(defn -main []
  (let [file-values (load-resource-file)]
    (println (run-program-val file-values))
    (println (run-program-mem file-values))))
