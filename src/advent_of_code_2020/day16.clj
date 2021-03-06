(ns advent-of-code-2020.day16
  (:gen-class)
  (:require [clojure.string :as str]
            [advent-of-code-2020.util :refer :all]))

(def data 
  (->> (str/split (slurp "resources/day16.raw") #"\n\n")
       (map #(str/split-lines %))))

(defn- parse-rule [rule]
  (let [[_ name val1 val2 val3 val4] (re-matches #"([a-z\s]+): (\d+)-(\d+) or (\d+)-(\d+)" rule)
        num1 (Long/parseLong val1) num2 (Long/parseLong val2) 
        num3 (Long/parseLong val3) num4 (Long/parseLong val4)]
    {name #(or (<= num1 % num2) (<= num3 % num4))}))

(defn- parse-rules [rules]
  (reduce #(conj %1 (parse-rule %2)) {} rules))

(defn- parse-ticket [ticket]
  (->> (str/split ticket #",")
       (map #(Long/parseLong %))))

(defn- parse [data]
  {:rules (parse-rules (first data))
   :mine (parse-ticket (second (second data)))
   :nearby (map parse-ticket (rest (nth data 2)))})

(defn- ticket-invalid? [rules]
  (apply every-pred (map complement (vals rules))))

(defn- step-one [[sum rules] ticket]
  (if-let [invalid (first (filter (ticket-invalid? rules) ticket))]
    [(+ sum invalid) rules]
    [sum rules]))

(defn- part-one [rules tickets]
  (first (reduce step-one [0 rules] tickets)))

(defn- step-two [poss ticket]
  (map #(filter-map [nam func] (func %2) %1) poss ticket))

(defn- step-two-b [[used values] entry]
  (let [[index names] entry
        value (first (remove used names))]
    [(conj used value) (conj values [index value])]))

(defn- part-two [rules mine nearby]
  (let [valids (filter #(empty? (filter (ticket-invalid? rules) %)) nearby)
        options (step-two (map (constantly rules) mine) mine)]
    (->> (reduce step-two options valids)
         (map-indexed #(vector %1 (keys %2)))
         (sort-by #(count (second %)))
         (reduce step-two-b [#{} []])
         (second)
         (filter #(str/includes? (second %) "departure"))
         (transduce (map #(nth mine (first %))) *))))

(defn -main []
  (let [ticket-data (parse data)]
    (println (part-one (:rules ticket-data) (:nearby ticket-data)))
    (println (part-two (:rules ticket-data) (:mine ticket-data) (:nearby ticket-data)))))
