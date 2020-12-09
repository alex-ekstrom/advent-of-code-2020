(ns advent-of-code-2020.day08
  (:gen-class)
  (:require [clojure.string :as str]
            [advent-of-code-2020.util :refer :all]))

(defn- next-instruction [state _]
  (update state :pos inc))

(defn- jump-instructions [state param]
  (update state :pos #(+ param %)))

(defn- store [state param]
  (update state :mem #(+ param %)))

(def instructions {:jmp jump-instructions
                   :acc #(-> (store %1 %2)
                             (next-instruction %2))
                   :nop next-instruction})

(defn- load-resource-file []
  (->> (str/split (slurp "resources/day08.raw") #"\n")
       (map #(str/split % #" "))
       (map #(hash-map :inst (keyword (first %)) :exec false :param (Integer/parseInt (second %))))
       (vec)))

(defn- run-ops
  ([inputs]
   (run-ops inputs {:pos 0 :mem 0 :state :running}))
  ([inputs state]
   (if (> (count inputs) (:pos state) -1)
     (let [current (nth inputs (:pos state))]
       (if (:exec current)
         (assoc state :state :inf)
         (let [next ((get instructions (:inst current)) state (:param current))]
           (recur (assoc-in inputs [(:pos state) :exec] true) next))))
     (assoc state :state :halt))))

(defn- find-loop [inputs]
  (:mem (run-ops inputs)))

(defn- invert-inst [inst]
  (case inst
    :acc :acc
    :jmp :nop
    :nop :jmp))

(defn- find-halt 
  ([inputs]
    (find-halt inputs 0))
  ([inputs index]
   (let [state (run-ops (update-in inputs [index :inst] invert-inst))]
     (if (= :halt (:state state))
       (:mem state)
       (recur inputs (inc index))))))


(defn -main []
  (let [file-values (load-resource-file)]
    (println (find-loop file-values))
    (println (find-halt file-values))))
