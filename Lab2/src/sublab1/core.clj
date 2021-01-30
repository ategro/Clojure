(ns sublab1.core)

(defn area
  [a b h]
  (* h (* 0.5 (+ a b))))

(defn integrate
  "Функция численно интегрирует функцию f от 0.0 до x с шагом step"
  [f step]
  {:pre [(>= step 0.0)]}
  (let [integrate-memoized (memoize
                             (fn fn [f n step rvalue]
                               (if (= n 0)
                                 0
                                 (let [lvalue (f (* (dec n) step))]
                                   (+
                                     (area lvalue rvalue step)
                                     (integrate-memoized f (dec n) step lvalue))))))]
    (fn [x]
      {:pre [(>= x 0.0)]}
      (let [n (int (quot x step))
            rvalue (f (* (dec n) step))]
        (integrate-memoized f n step rvalue)))))

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
