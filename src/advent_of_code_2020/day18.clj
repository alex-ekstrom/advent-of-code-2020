(ns advent-of-code-2020.day18
  (:gen-class)
  (:require [clojure.string :as str]
            [advent-of-code-2020.util :refer :all]))

(def data 
  (str/split-lines (slurp "resources/day18.raw")))

(defn- map-char [ch]
  (case ch
    "(" \(
    ")" \)
    (load-string ch)))

(defn- parse [data]
  (->> (map #(str/split % #"") data)
       (map #(remove #{" "} %))
       (map #(map map-char %))))

(defn- calc-no-prec [[values operators] value]
  (cond (number? value)
          (if (or (= (peek operators) \() (empty? operators))
            [(conj values value) operators]
            [(conj (pop values) ((peek operators) (peek values) value)) (pop operators)])
        (fn? value) [values (conj operators value)]
        (= \( value) [values (conj operators value)]
        (= \) value) (if (fn? (peek (pop operators)))
                       [(conj (pop (pop values)) ((peek (pop operators)) (peek values) (peek (pop values)))) (pop (pop operators))]
                       [values (pop operators)])))

(defn- empty-parens [values operators]
  (if (= (peek operators) \()
    [values operators]
    (recur (conj (pop (pop values)) ((peek operators) (peek values) (peek (pop values)))) (pop operators))))

(defn- calc-with-prec [[values operators] value]
  (cond (number? value)
          (if (or (= (peek operators) \() (= (peek operators) *) (empty? operators))
            [(conj values value) operators]
            [(conj (pop values) ((peek operators) (peek values) value)) (pop operators)])
        (fn? value) [values (conj operators value)]
        (= \( value) [values (conj operators value)]
        (= \) value) (let [[values operators] (empty-parens values operators)]
                       (if (= (peek (pop operators)) +)
                         [(conj (pop (pop values)) ((peek (pop operators)) (peek values) (peek (pop values)))) (pop (pop operators))]
                         [values (pop operators)]))))

(defn part-one [ops]
  (get-in (reduce calc-no-prec [[] []] ops) [0 0]))

(defn part-two [ops]
  (prn (first (reduce calc-with-prec [[] []] ops)))
  (reduce * (first (reduce calc-with-prec [[] []] ops))))

(defn- sum [ops prob]
  (transduce (map prob) + ops))

(def test-data ["1 + 2 * 3 + 4 * 5 + 6"])

(defn -main []
  (let [test-data (parse test-data)
        input-data (parse data)]
    (println (sum test-data part-one))
    (println (sum input-data part-one)) 
    (println (sum test-data part-two))
    (println (sum input-data part-two))))
