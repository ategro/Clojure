(ns sublab1.core)

(defn area
  [a b h]
  (* h (* 0.5 (+ a b))))

(def integrate-memoized
      (memoize
        (fn [f n step rvalue]
          (if (= n 0)
            0
            (let [lvalue (f (* (dec n) step))]
              (+
                (area lvalue rvalue step)
                (integrate-memoized f (dec n) step lvalue)))))))

(defn slow-sqr
  [x]
  (Math/sin (Math/asin (Math/sin (Math/asin (Math/pow x 2.0))))))

(defn integrate
  "Функция численно интегрирует функцию f от 0.0 до x с шагом step"
  [f step]
  {:pre [(>= step 0.0)]}
  (fn [x]
    {:pre [(>= x 0.0)]}
    (let [n (int (quot x step))
          rvalue (f (* (dec n) step))]
      (integrate-memoized f n step rvalue))))

(defn main [& args]
  (let [integrate-function (integrate slow-sqr 0.01)]
    (time (integrate-function 1))
    (time (integrate-function 0.25))
    (time (integrate-function 0.5))
    (time (integrate-function 1))))
