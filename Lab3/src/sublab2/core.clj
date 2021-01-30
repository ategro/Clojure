(ns sublab2.core)

(defn my-filter-parallel
  [predicate coll number-of-elements-per-thread]
  (defn- lazy-chunk-parts
    [coll number-of-elements]
    (if (empty? coll)
      nil
      (let [head (take number-of-elements coll)
            tail (drop number-of-elements coll)]
        (lazy-seq
          (cond
            (empty? head) nil
            (empty? tail) (cons head nil)
            :else (cons head (lazy-chunk-parts tail number-of-elements)))))))
  (let [futures (doall (map #(future (doall (filter predicate %)))
                            (lazy-chunk-parts coll number-of-elements-per-thread)))]
    (reduce concat (map deref futures))))

(defn my-filter
  "Ленивый параллельный filter"
  [predicate coll number-of-threads number-of-elements-per-thread]
  {:pre [(> number-of-threads 0)
         (> number-of-elements-per-thread 0)]}
  (if (empty? coll)
    '()
    (let [chunk-size (* number-of-threads number-of-elements-per-thread)
          head (take chunk-size coll)
          tail (drop chunk-size coll)]
      (concat
        (my-filter-parallel predicate head number-of-elements-per-thread)
        (lazy-seq (my-filter predicate tail number-of-threads number-of-elements-per-thread))))))

(defn slow-function
  [x]
  (Thread/sleep 1)
  (even? x))

(def nature (iterate inc 0))

(defn run-tests []
  (def tests
    (list
      (= (doall (take 1000 (my-filter slow-function nature 4 101)))
         (doall (take 1000 (filter slow-function nature))))
      (= (doall (take 1000 (my-filter slow-function '() 4 101)))
         (doall (take 1000 (filter slow-function '()))))
      (= (doall (take 1000 (my-filter slow-function '(1 2 3) 4 101)))
         (doall (take 1000 (filter slow-function '(1 2 3)))))
      ))
  (println tests)
  (println
    (if (every? true? tests)
      "All tests passed"
      "One or more tests failed")))

(defn main [& args]
  (time (doall (take 1000 (filter slow-function nature))))
  (time (doall (take 1000 (my-filter slow-function nature 1 101))))
  (time (doall (take 1000 (my-filter slow-function nature 8 130))))
  (time (doall (take 5000 (filter slow-function nature))))
  (time (doall (take 5000 (my-filter slow-function nature 8 125))))
  '(true)
  )