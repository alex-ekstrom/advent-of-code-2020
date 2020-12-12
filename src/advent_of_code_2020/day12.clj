(ns advent-of-code-2020.day12
  (:gen-class)
  (:require [clojure.string :as str]
            [advent-of-code-2020.util :refer :all])
  (:import [java.lang Math]))

(defn- apply-vec [[x y :as loc] rot dist]
  [(+ x (Math/round (* (Math/cos rot) dist)))
   (+ y (Math/round (* (Math/sin rot) dist)))])

(defn- translate [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn- move 
  ([dir dist loc rot speed]
   [(apply-vec loc dir dist)
    rot
    speed])
  ([dir dist loc rot speed loc2]
   [loc rot speed
    (apply-vec loc2 dir dist)]))

(defn- forward 
  ([dist loc rot speed]
   [(apply-vec loc rot dist)
    rot
    dist])
  ([dist [x1 y1 :as loc] rot speed [x2 y2 :as loc2]]
   (loop [loc loc diff [(- x2 x1) (- y2 y1)] times dist]
     (if (= 0 times)
       [loc rot speed (translate loc diff)]
       (recur (translate loc diff) diff (dec times))))))

(defn- rotate 
  ([rad loc rot speed]
   [loc (+ rot rad) speed])
  ([rad [x1 y1 :as loc] rot speed [x2 y2]]
   (let [s (Math/sin rad)
         c (Math/cos rad)
         [x3 y3] [(- x2 x1) (- y2 y1)]
         [x4 y4] [(- (* x3 c) (* y3 s)) (+ (* x3 s) (* y3 c))]]
       [loc rot speed [(Math/round (+ x4 x1)) (Math/round (+ y4 y1))]])))

(def directions {:E #(partial move 0 %)
                 :S #(partial move (Math/toRadians 270) %)
                 :W #(partial move (Math/toRadians 180) %)
                 :N #(partial move (Math/toRadians 90) %)
                 :R #(partial rotate (Math/toRadians (- 0 %)))
                 :L #(partial rotate (Math/toRadians %))
                 :F #(partial forward %)})

(defn- load-resource-file []
  (->> (slurp "resources/day12.raw")
       (str/split-lines)
       (map #((get directions (keyword (subs % 0 1))) (Long/parseLong (subs % 1))))))

(defn- process-directions [dirs]
  (loop [insts dirs loc [0 0] rot 0 speed 0]
    (if-let [[inst & rest] insts]
      (let [[loc rot speed] (inst loc rot speed)]
        (recur rest loc rot speed))
      (+ (Math/abs (first loc)) (Math/abs (second loc))))))

(defn- process-waypoints [dirs]
  (loop [insts dirs loc [0 0] rot 0 speed 0 loc2 [10 1]]
    (if-let [[inst & rest] insts]
      (let [[loc rot speed loc2] (inst loc rot speed loc2)]
        (recur rest loc rot speed loc2))
      (+ (Math/abs (first loc)) (Math/abs (second loc))))))

(defn -main []
  (let [file-values (load-resource-file)]
    (println (process-directions file-values))
    (println (process-waypoints file-values))))
