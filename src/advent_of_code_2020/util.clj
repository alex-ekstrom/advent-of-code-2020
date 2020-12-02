(ns advent-of-code-2020.util)

(defn xor [& args]
  (if (= 1 (get (frequencies args) true))
    true
    false))
