(ns sublab1.core)

(def enable-traces false)

(defn trace
  "Вывод промежуточных логов"
  [& expr]
  (if (= enable-traces true)
    (apply println expr)
    nil))

(defn op-args
  "Получение списка аргументов терма"
  [expr]
  (rest expr))

(defn constant
  "Конструктор булевой константы `value` (0 или 1)"
  [value]
  {:pre [(or (= value 0) (= value 1))]}
  (list ::constant value))

(defn constant?
  "Проверка, является ли терм булевой константой"
  [expr]
  (= ::constant (first expr)))

(defn constant-value
  "Получение значения булевой константы"
  [expr]
  (second expr))

(defn variable
  "Конструктор именованной переменной"
  [name]
  {:pre [(keyword? name)]}
  (list ::variable name))

(defn variable?
  "Проверка, является ли терм именованной переменной"
  [expr]
  (= ::variable (first expr)))

(defn variable-name
  "Получение имени именованной переменной"
  [expr]
  (second expr))

(defn op-not
  "Конструктор логического НЕ"
  [expr]
  (list ::not expr))

(defn op-not?
  "Проверка, является ли терм логическим НЕ"
  [expr]
  (= ::not (first expr)))

(defn op-not-arg
  "Получение аргумента терма"
  [expr]
  (first (op-args expr)))

(defn op-or
  "Конструктор логического ИЛИ"
  [a b & expr]
  (cons ::or (cons a (cons b expr))))

(defn op-or?
  "Проверка, является ли терм логическим ИЛИ"
  [expr]
  (= ::or (first expr)))

(defn op-and
  "Конструктор логического И"
  [a b & expr]
  (cons ::and (cons a (cons b expr))))

(defn op-and?
  "Проверка, является ли терм логическим И"
  [expr]
  (= ::and (first expr)))

(defn op-implication
  "Конструктор логической импликации"
  [a b]
  (cons ::-> (list a b)))

(defn op-implication?
  "Проверка, является ли терм импликацией"
  [expr]
  (= ::-> (first expr)))

(defn to-dnf-operator
  [pred rule]
  (defn- apply-of-rule [expr]
    (cond
      ; Константы оставляем как есть
      (constant? expr) expr
      ; Переменные оставляем как есть
      (variable? expr) expr
      :else (let [modified-expr (cons (first expr) (map apply-of-rule (op-args expr)))]
              (if (pred modified-expr)
                (rule modified-expr)
                modified-expr)))))

(def dnf-rules
  (list
    ; A -> B = NOT A OR B
    (fn [expr]
      (trace "> A -> B = NOT A OR B")
      ((to-dnf-operator
        op-implication?
        #(let [arg (op-args %)]
           (trace % "\n-> " (op-or (op-not (first arg)) (second arg)))
           (op-or
             (op-not (first arg))
             (second arg)))) expr))

    ; Законы де Моргана
    (fn [expr]
      (defn- de-morgan-predicate
        [expr]
        (and (op-not? expr)
             (or (op-or? (op-not-arg expr))
                 (op-and? (op-not-arg expr)))))
      (defn- de-morgan-transform
        [expr]
        (if (de-morgan-predicate expr)
          (let [arg (op-not-arg expr)
                op (if (op-or? arg) op-and op-or)
                result (apply op (map #(de-morgan-transform (op-not %)) (op-args arg)))]
            (trace expr "\n-> " result)
            result)
          expr))
      (trace "> Законы де Моргана")
      ((to-dnf-operator
         de-morgan-predicate
         de-morgan-transform) expr))

    ; NOT NOT A = A
    (fn [expr]
      (trace "> NOT NOT A = A")
      ((to-dnf-operator
         #(and (op-not? %)
               (op-not? (op-not-arg %)))
         #(do
            (trace % "\n-> " (op-not-arg (op-not-arg %)))
            (op-not-arg (op-not-arg %)))) expr))
    
    ; Отрицания констант заменяем константами
    (fn [expr]
      (trace "> NOT C = 1 - C")
      ((to-dnf-operator
         #(and (op-not? %) (constant? (op-not-arg %)))
         #(do
            (trace % "\n-> " (constant (- 1 (constant-value (op-not-arg %)))))
            (constant (- 1 (constant-value (op-not-arg %)))))) expr))

    ; Упрощение выражений с константами
    (fn [expr]
      (trace "> Упрощение выражений с константами")
      ((to-dnf-operator
         #(and (or (op-or? %)
                   (op-and? %))
               (= (some constant? (op-args %)) true))
         #(let [result (let [args (op-args %)
                             non-constant-args (filter (fn [e] (not (constant? e))) args)
                             non-constant-args-size (count non-constant-args)
                             constant-args (filter constant? args)
                             base-operation (if (op-or? %) op-or op-and)
                             value-constant (if (op-or? %) 0 1)]
                         (if (empty? (filter (fn [c] (= (constant-value c) (- 1 value-constant))) constant-args))
                           (if (> non-constant-args-size 1)
                             (apply base-operation non-constant-args)
                             (if (= non-constant-args-size 1)
                               (first non-constant-args)
                               (constant value-constant)))
                           (constant (- 1 value-constant))))]
            (trace % "\n-> " result)
            result))
       expr))

    ; AND (AND) || OR (OR) || AND (OR)
    (fn [expr]
      (defn- predicate
        [expr]
        (or (and (op-and? expr)
                 (or (= (some op-and? (op-args expr)) true)
                     (= (some op-or? (op-args expr)) true)))
            (and (op-or? expr)
                 (= (some op-or? (op-args expr)) true))))
      (defn- duplicate-op-transform
        [expr op op-pred]
        (if (op-pred expr)
          (let [args (op-args expr)
                result-args (reduce
                              (fn [coll arg]
                                (if (op-pred arg)
                                  (concat coll (op-args arg))
                                  (concat coll (cons arg nil))))
                              ()
                              args)]
            (apply op result-args))
          expr))
      (defn- and-or-transform
        [expr]
        (let [args (op-args expr)
              or-args (map #(map (fn [e] (cons e nil)) (op-args %)) (filter op-or? args))
              non-or-args (list (mapcat #(cons % nil) (filter #(not (op-or? %)) args)))
              args-for-reduce (concat (map list non-or-args) or-args)
              permutations-of-args (reduce (fn [permutations args]
                                             (for [x permutations
                                                   y args]
                                               (concat x y)))
                                           args-for-reduce)]
          (apply op-or
                 (map #(duplicate-op-transform (apply op-and %) op-and op-and?)
                      permutations-of-args))))
      (defn- transform
        [expr]
        (let [modified-expr (duplicate-op-transform (duplicate-op-transform expr op-or op-or?) op-and op-and?)]
          (if (predicate modified-expr)
            (and-or-transform modified-expr)
            modified-expr)))
      (trace "> AND (AND) || OR (OR) || AND (OR)")
      ((to-dnf-operator
         predicate
         #(let [result (transform %)]
            (trace % "\n-> " result)
            result)) expr))
    ))

(defn to-dnf
  "Преобразование термов в ДНФ"
  ([expr] (to-dnf expr dnf-rules))
  ([expr rules]
   (if (empty? rules)
     expr
     (to-dnf ((first rules) expr) (rest rules)))))

(defn to-dnf-with-replacement
  [expr variables-value]
  (to-dnf
    (doall
      ((defn- apply-of-rule [expr]
         (cond
           ; Константы оставляем как есть
           (constant? expr)
           expr
           ; Переменные оставляем как есть
           (variable? expr)
           (let [name (variable-name expr)
                 value (name variables-value)]
             (if (not (= value nil))
               (constant value)
               expr))
           :else
           (let [modified-expr (cons (first expr) (map apply-of-rule (op-args expr)))]
             modified-expr))) expr))))

(defn run-tests []
  (def tests
    (list
      (= (to-dnf (constant 0))
         (constant 0))
      (= (to-dnf (constant 1))
         (constant 1))
      (= (to-dnf (variable :a))
         (variable :a))
      (= (to-dnf (op-not (constant 1)))
         (constant 0))
      (= (to-dnf (op-not (op-not (constant 0))))
         (constant 0))
      (= (to-dnf (op-implication (constant 1) (variable :b)))
         (variable :b))
      (= (to-dnf (op-implication (constant 1) (constant 0)))
         (constant 0))
      (= (to-dnf (op-not (op-and (constant 0) (variable :a) (variable :b))))
         (constant 1))
      (= (to-dnf (op-not (op-and  (variable :a) (op-or (variable :b) (variable :c)))))
         (op-or (op-not (variable :a)) (op-and (op-not (variable :b)) (op-not (variable :c)))))
      (= (to-dnf (op-or (op-or (variable :a) (variable :b)) (op-or (variable :c) (variable :d))))
         (op-or (variable :a) (variable :b) (variable :c) (variable :d)))
      (= (to-dnf (op-and (op-and (variable :a) (variable :b)) (op-and (variable :c) (variable :d))))
         (op-and (variable :a) (variable :b) (variable :c) (variable :d)))
      (= (to-dnf-with-replacement (op-and (op-and (variable :a) (variable :b)) (op-and (variable :c) (variable :d)))
                                  {:a 1 :b 1 :c 1})
         (variable :d))
      (= (to-dnf-with-replacement (to-dnf (op-and (variable :a) (op-or (variable :b) (variable :c))))
                                  {:b 1 :c 0})
         (variable :a))
      ))
  (println tests)
  (println
    (if (every? true? tests)
      "All tests passed"
      "One or more tests failed")))

(defn main [& op-args]
  (to-dnf (op-and (op-or (variable :a1)
                         (variable :b1)
                         (variable :c1))
                  (op-or (variable :a2)
                         (variable :b2)
                         (variable :c2))
                  (op-or (variable :a3)
                         (variable :b3)
                         (variable :c3))
                  (variable :d4))))