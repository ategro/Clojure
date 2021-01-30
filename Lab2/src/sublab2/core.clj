(ns sublab2.core)

(defn area
  [a b h]
  (* h (* 0.5 (+ a b))))

(defn integrated-value
  ([f step]
    (integrated-value f 0 step 0.0))
  ([f n step sum]
   (let [value (+ sum (area (f (* step (dec n))) (f (* step n)) step))]
     (lazy-seq (cons value (integrated-value f (inc n) step value))))))

(defn slow-sqr
  [x]
  (Math/sin (Math/asin (Math/sin (Math/asin (Math/pow x 2.0))))))

(defn integrate
  "Функция численно интегрирует функцию f от 0.0 до x с шагом step"
  [f step]
  {:pre [(>= step 0.0)]}
  (fn [x]
    {:pre [(>= x 0.0)]}
    (let [n (int (quot x step))]
      (if (= n 0)
        0.0
        (nth (integrated-value f step) (dec n))))))

; (nth last (take n (integrated-value f step)))

(defn main [& args]
  (let [integrate-function (integrate slow-sqr 0.007)]
    (time (integrate-function 1))
    (time (integrate-function 0.25))
    (time (integrate-function 0.5))
    (time (integrate-function 1))))
