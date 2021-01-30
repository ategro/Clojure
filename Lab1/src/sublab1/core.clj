(ns sublab1.core)

(defn generate-permutations
  "Генерирует список всех строк длины n с заданным алфавитом и не содержащих двух одинаковых букв, идущих подряд"
  [alphabet n]
  {:pre [(>= n 0)
         (= (count alphabet) (count (distinct alphabet)))]}
  (defn- generate [m]
    (cond
      (= m 0) '()
      (= m 1) (for [character alphabet]
                (cons character nil))
      :else (for [tail (generate (dec m))
                  head alphabet
                  :when (not= head (first tail))]
              (cons head tail))))
  (for [x (generate n)]
    (apply str x)))

(defn main [& args]
  (println (generate-permutations (seq "abc") 0))
  (println (generate-permutations (seq "abc") 1))
  (println (generate-permutations (seq "abc") 2))
  (println (generate-permutations (seq "abc") 3)))