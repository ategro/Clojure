(ns sublab2.core)

(defn generate-permutations
  "Генерирует список всех строк длины n с заданным алфавитом и не содержащих двух одинаковых букв, идущих подряд"
  [alphabet n]
  {:pre [(>= n 0)
         (= (count alphabet) (count (distinct alphabet)))]}
  (defn- generate [m local-alphabet initial-permutations result-permutations first-call]
    (cond
      (= m 0) initial-permutations
      (and (empty? local-alphabet) (empty? initial-permutations))
        (generate (dec m) alphabet result-permutations '() false)
      (empty? local-alphabet)
        (generate m alphabet (rest initial-permutations) result-permutations first-call)
      (and (empty? initial-permutations) (not first-call))
        (generate (dec m) alphabet result-permutations '() false)
      :else
        (if (= (first local-alphabet) (first (first initial-permutations)))
          (generate m (rest local-alphabet) initial-permutations result-permutations first-call)
          (recur m (rest local-alphabet) initial-permutations
                 (cons (cons (first local-alphabet) (first initial-permutations)) result-permutations) first-call))))
  (defn- to-word [characters words]
    (if (empty? characters)
      words
      (recur (rest characters)
             (cons (apply str (first characters)) words))))
  (to-word (generate n alphabet '() '() true) '()))

(defn main [& args]
  (println (generate-permutations (seq "abc") 0))
  (println (generate-permutations (seq "abc") 1))
  (println (generate-permutations (seq "abc") 2))
  (println (generate-permutations (seq "abc") 3)))