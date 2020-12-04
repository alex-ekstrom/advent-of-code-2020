(ns advent-of-code-2020.day04
  (:gen-class)
  (:require [clojure.string :as str]
            [advent-of-code-2020.util :refer :all]))

(defn height-valid? [str-val]
  (if-let [value (re-find (re-matcher #"(\d+)(in|cm)" str-val))]
    (case (nth value 2)
      "in" (<= 59 (Integer/parseInt (second value)) 76)
      "cm" (<= 150 (Integer/parseInt (second value)) 193))
    false))

(def valid-values {"byr" #(and (re-matches #"\d{4}" %) (<= 1920 (Integer/parseInt %) 2002))
                   "iyr" #(and (re-matches #"\d{4}" %) (<= 2010 (Integer/parseInt %) 2020))
                   "eyr" #(and (re-matches #"\d{4}" %) (<= 2020 (Integer/parseInt %) 2030))
                   "hgt" #(height-valid? %)
                   "hcl" #(re-matches #"#[0-9a-f]{6}" %)
                   "ecl" #(re-matches #"(amb|blu|brn|gry|grn|hzl|oth)" %)
                   "pid" #(re-matches #"[0-9]{9}" %)
                   "cid" (fn [_] true)})

(defn map-passport [str-val]
  (->> (re-seq #"([a-z]+):([^\s]+)" str-val)
       (map rest)
       (map #(apply hash-map %))
       (into {})))

(defn- load-resource-file []
  (->> (str/split (slurp "resources/day04.raw") #"\n\n")
       (map map-passport)))

(defn is-valid-present? [passport]
  (every? passport
          ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]))

(defn is-valid-value? [passport]
  (and (is-valid-present? passport)
       (every? #((get valid-values (first %)) (second %)) passport)))

(defn- count-valid [inputs method]
  (->> inputs
       (filter method)
       (count)))

(defn -main []
  (let [file-values (load-resource-file)]
    (println (count-valid file-values is-valid-present?))
    (println (count-valid file-values is-valid-value?))))
