(ns sublab4.core)

(defn generate-permutations
  "Генерирует список всех строк длины n с заданным алфавитом и не содержащих двух одинаковых букв, идущих подряд"
  [alphabet n]
  {:pre [(>= n 0)
         (= (count alphabet) (count (distinct alphabet)))]}
  (defn- generate [m]
    (cond
      (= m 0) '()
      (= m 1) (map #(cons % nil) alphabet)
      :else (reduce
              (fn [a b]
                (concat a
                        (filter #(not= (first %) (second %))
                                (map #(cons % b) alphabet))))
              '()
              (generate (dec m)))))
  (for [x (generate n)]
    (apply str x)))

(defn main [& args]
  (println (generate-permutations (seq "abc") 0))
  (println (generate-permutations (seq "abc") 1))
  (println (generate-permutations (seq "abc") 2))
  (println (generate-permutations (seq "abc") 3)))