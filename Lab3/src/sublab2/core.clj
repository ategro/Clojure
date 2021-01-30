(ns sublab2.core)

(defn my-fillter-parallel
  [predicate coll number-of-elements-per-thread]
  (defn- lazy-batch-parts
    [coll number-of-elements]
    (if (empty? coll)
      nil
      (let [head (take number-of-elements coll)
            tail (drop number-of-elements coll)]
        (lazy-seq
          (cond
            (empty? head) nil
            (empty? tail) (cons head nil)
            :else (cons head (lazy-batch-parts tail number-of-elements)))))))
  (let [futures (doall (map #(future (do (println "Thread") (doall (filter predicate %)))) (lazy-batch-parts coll number-of-elements-per-thread)))]
    (reduce concat (map deref futures))))

(defn my-filter
  "Ленивый параллельный filter"
  [predicate coll number-of-threads number-of-elements-per-thread ]
  {:pre [(> number-of-threads 0)
         (> number-of-elements-per-thread 0)]}
  (if (empty? coll)
    '()
    (let [batch-size (* number-of-threads number-of-elements-per-thread)
          head (take batch-size coll)
          tail (drop batch-size coll)]
      (concat
        (my-fillter-parallel predicate head number-of-elements-per-thread)
        (lazy-seq (my-filter predicate tail number-of-threads number-of-elements-per-thread))))))

(defn slow-function
  [x]
  (Thread/sleep 1)
  (even? x))

(def nature (iterate inc 0))

(defn main [& args]
  ;(time (doall (take 1000 (filter slow-function nature))))
  ;(time (doall (take 1000 (my-filter slow-function nature 1 100))))
  ;(time (doall (take 1000 (my-filter slow-function nature 8 125))))
  ;(time (doall (take 5000 (my-filter slow-function nature 1 100))))
  ;(time (doall (take 5000 (my-filter slow-function nature 8 125))))
  (time (doall (take 100 (my-filter slow-function nature 8 1))))
  '(true)
  )