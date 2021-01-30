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
  (let [integrated-value-seq (integrated-value f step)]
    (fn [x]
      {:pre [(>= x 0.0)]}
      (let [n (int (quot x step))]
        (nth integrated-value-seq n)))))

(defn slow-function
  [v]
  (fn [x]
    (Thread/sleep 1)
    v))

(defn main [& args]
  (let [integrate-function (integrate (slow-function 4) 0.01)
        integrate-function2 (integrate (slow-function 8) 0.01)]
    (time (integrate-function 1))
    (time (integrate-function 0.25))
    (time (integrate-function 0.5))
    (time (integrate-function 1))

    (println (integrate-function 0.00))
    (println (integrate-function 0.25))
    (println (integrate-function 0.50))
    (println (integrate-function 0.75))
    (println (integrate-function 1.00))

    (time (integrate-function2 1))
    (time (integrate-function2 0.25))
    (time (integrate-function2 0.5))
    (time (integrate-function2 1))

    (println (integrate-function2 0.00))
    (println (integrate-function2 0.25))
    (println (integrate-function2 0.50))
    (println (integrate-function2 0.75))
    (println (integrate-function2 1.00))))


