(ns sublab3.core)

(defn my-map
  "Функция аналогичная map (для одного списка)"
  [f coll]
  (defn- func [a b]
    (concat a (cons (f b) nil)))
  (reduce func '() coll))

(defn my-filter
  "Функция аналогичная filter"
  [f coll]
  (defn- func [a b]
    (if (f b)
      (concat a (cons b nil))
      a))
  (reduce func '() coll))

(defn main [& args]
  (println (range 10) " map -> " (my-map inc (range 10)))
  (println (range 10) " filter -> " (my-filter even? (range 10))))