(ns advent-of-code-2020.util)

(defn xor [& args]
  (if (= 1 (get (frequencies args) true))
    true
    false))

(defmacro filter-map [bindings pred m]
  `(select-keys ~m
    (for [~bindings ~m
      :when ~pred]
      ~(first bindings))))
