(ns sublab2.core)

(defn area
  [a b h]
  (* h (* 0.5 (+ a b))))

(defn integrated-value
  ([f step]
    (integrated-value f step 0 0.0))
  ([f step n sum]
   (let [value (if (= n 0)
                 0.0
                 (+ sum (area (f (* step (dec n))) (f (* step n)) step)))]
     (lazy-seq (cons value (integrated-value f step (inc n) value))))))

(defn integrate
  "Функция численно интегрирует функцию f от 0.0 до x с шагом step"
  [f step]
  {:pre [(>= step 0.0)]}
  (def integrated-value-seq (integrated-value f step))
  (fn [x]
    {:pre [(>= x 0.0)]}
    (let [n (int (quot x step))]
      (nth integrated-value-seq n))))

(defn slow-function
  [x]
  (Thread/sleep 1)
  1)

(defn main [& args]
  (let [integrate-function (integrate slow-function 0.01)]
    (time (integrate-function 1))
    (time (integrate-function 0.25))
    (time (integrate-function 0.5))
    (time (integrate-function 1))

    (println (integrate-function 0.00))
    (println (integrate-function 0.25))
    (println (integrate-function 0.50))
    (println (integrate-function 0.75))
    (println (integrate-function 1.00))))


