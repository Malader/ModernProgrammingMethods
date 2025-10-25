(ns lab4_dnf
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn VAR  "Создать переменную (символ)." [sym] sym)
(defn BOOL "Создать булеву константу true/false." [b] (boolean b))
(defn NOT  "Отрицание: {:op :not, :args [x]}." [x] {:op :not :args [x]})
(defn AND  "Конъюнкция: {:op :and, :args [...]}." [& xs] {:op :and :args (vec xs)})
(defn OR   "Дизъюнкция: {:op :or,  :args [...]}." [& xs] {:op :or  :args (vec xs)})
(defn IMP  "Импликация: {:op :imp, :args [A B]}." [a b] {:op :imp :args [a b]})
(defn XOR [a b] {:op :xor  :args [a b]})
(defn NOR [a b] {:op :nor  :args [a b]})
(defn NAND[a b] {:op :nand :args [a b]})
(defn TWOOF3 [a b c] {:op :twoof3 :args [a b c]})
(defn op-node? [e] (and (map? e) (contains? e :op)))
(defn op-of    [e] (when (op-node? e) (:op e)))
(defn args-of  [e] (when (op-node? e) (:args e)))

(def ^:private derived-reg (atom {}))

(defn register-derived-op!
  "Зарегистрировать преобразование производной операции k к базовым (and/or/not)."
  [k lower-fn]
  (swap! derived-reg assoc k lower-fn))

(register-derived-op! :xor    (fn [[a b]] (OR (AND a (NOT b)) (AND (NOT a) b))))
(register-derived-op! :nor    (fn [[a b]] (NOT (OR a b))))
(register-derived-op! :nand   (fn [[a b]] (NOT (AND a b))))
(register-derived-op! :twoof3 (fn [[a b c]]
                                (OR (AND a b (NOT c))
                                    (AND a c (NOT b))
                                    (AND b c (NOT a)))))

(declare desugar simplify nnf nnf-neg)

(defn desugar
  "Понизить производные операции (:imp и зарегистрированные) к базовым :and/:or/:not."
  [e]
  (cond
    (boolean? e) e
    (symbol?  e) e
    (op-node? e)
    (let [op (op-of e) as (map desugar (args-of e))]
      (case op
        :imp (desugar (OR (NOT (first as)) (second as)))
        :not (NOT (first as))
        :and (apply AND as)
        :or  (apply OR  as)
        (if-let [f (@derived-reg op)]
          (desugar (f as))
          (throw (ex-info "Unknown op" {:op op})))))
    :else (throw (ex-info "Bad expr" {:expr e}))))

(defn subst
  "Подстановка env (символ→булево). Неприсутствующие символы остаются переменными."
  [e env]
  (cond
    (boolean? e) e
    (symbol?  e) (if (contains? env e) (BOOL (env e)) e)
    (op-node? e) (let [as (map #(subst % env) (args-of e))]
                   {:op (op-of e) :args (vec as)})
    :else (throw (ex-info "Bad expr" {:expr e}))))

(defn flatten-op
  "Расплющить вложенные однотипные операции (ассоциативность)."
  [k xs]
  (mapcat (fn [x] (if (and (op-node? x) (= (op-of x) k)) (args-of x) [x])) xs))

(defn simplify
  "Локальные упрощения: константы, ¬¬X, расплющивание, дубликаты."
  [e]
  (cond
    (boolean? e) e
    (symbol?  e) e
    (op-node? e)
    (let [op (op-of e) as (map simplify (args-of e))]
      (case op
        :not (let [x (first as)]
               (cond
                 (boolean? x) (not x)
                 (and (op-node? x) (= (op-of x) :not)) (simplify (first (args-of x)))
                 :else (NOT x)))
        :and (let [xs (flatten-op :and as) xs (distinct xs) xs (remove true? xs)]
               (if (some false? xs) false
                   (case (count xs) 0 true 1 (first xs) (apply AND xs))))
        :or  (let [xs (flatten-op :or  as) xs (distinct xs) xs (remove false? xs)]
               (if (some true? xs) true
                   (case (count xs) 0 false 1 (first xs) (apply OR xs))))
        :imp (simplify (OR (NOT (first as)) (second as)))
        (simplify (desugar {:op op :args (vec as)}))))
    :else (throw (ex-info "Bad expr" {:expr e}))))

(defn nnf
  "НФ по отрицаниям (¬ только над переменными/константами)."
  [e]
  (let [e (simplify (desugar e))]
    (cond
      (boolean? e) e
      (symbol?  e) e
      (op-node? e)
      (case (op-of e)
        :not (nnf-neg (first (args-of e)))
        :and (apply AND (map nnf (args-of e)))
        :or  (apply OR  (map nnf (args-of e)))
        (throw (ex-info "Unexpected op in NNF" {:op (op-of e)})))
      :else (throw (ex-info "Bad expr" {:expr e})))))

(defn nnf-neg
  "NNF для выражения под внешним отрицанием."
  [e]
  (let [e (simplify e)]
    (cond
      (boolean? e) (not e)
      (symbol?  e) (NOT e)
      (op-node? e)
      (case (op-of e)
        :not (nnf (first (args-of e)))
        :and (apply OR  (map nnf-neg (args-of e)))
        :or  (apply AND (map nnf-neg (args-of e)))
        (throw (ex-info "Unexpected op in nnf-neg" {:op (op-of e)})))
      :else (throw (ex-info "Bad expr" {:expr e})))))

(defn literal?
  "Литерал: Var, ¬Var или булева константа."
  [e]
  (or (symbol? e)
      (and (op-node? e) (= (op-of e) :not) (symbol? (first (args-of e))))
      (boolean? e)))

(defn clauses
  "NNF → список конъюнктов (каждый — список литералов) для ДНФ."
  [e]
  (let [e (simplify e)]
    (cond
      (= e true)  [[]]
      (= e false) []
      (literal? e) [[e]]
      (and (op-node? e) (= (op-of e) :or))  (into [] (mapcat clauses (args-of e)))
      (and (op-node? e) (= (op-of e) :and))
      (reduce (fn [acc sub] (for [c1 acc, c2 (clauses sub)] (concat c1 c2)))
              [[]] (args-of e))
      :else (throw (ex-info "Expected NNF node" {:expr e})))))

(defn lit-key [l] (if (symbol? l) [:pos l] [:neg (first (args-of l))]))

(defn simplify-conj
  "Упростить один конъюнкт: убрать дубли, проверить A ∧ ¬A, отсортировать."
  [conj]
  (let [pos (set (map second (filter #(= :pos (first %)) (map lit-key conj))))
        neg (set (map second (filter #(= :neg (first %)) (map lit-key conj))))]
    (when (empty? (set/intersection pos neg))
      (->> conj distinct
           (sort-by (fn [l] (let [[sgn v] (lit-key l)] [v (if (= sgn :pos) 0 1)])))))))

(defn build-dnf
  "Собрать AST ДНФ из списка конъюнктов."
  [cls]
  (let [cls (keep simplify-conj cls) cls (distinct cls)]
    (cond
      (empty? cls) false
      (some empty? cls) true
      (= 1 (count cls)) (apply AND (first cls))
      :else (apply OR (map #(apply AND %) cls)))))

(defn to-dnf
  "expr → (desugar → simplify → nnf → clauses → build-dnf → simplify)."
  [expr]
  (-> expr desugar simplify nnf clauses build-dnf simplify))

(defn substitute->dnf
  "Подстановка env и приведение к ДНФ."
  [expr env]
  (to-dnf (subst expr env)))

(defn dnf?
  "Проверка, что выражение — в ДНФ."
  [e]
  (let [e (simplify e)]
    (cond
      (boolean? e) true
      (literal? e) true
      (and (op-node? e) (= (op-of e) :and)) (every? literal? (args-of e))
      (and (op-node? e) (= (op-of e) :or))
      (every? (fn [t] (or (literal? t)
                          (and (op-node? t) (= (op-of t) :and) (every? literal? (args-of t)))))
              (args-of e))
      :else false)))

(defn vars-of
  "Множество переменных (символов), встречающихся в выражении."
  [e]
  (cond
    (boolean? e) #{}
    (symbol?  e) #{e}
    (op-node? e) (apply set/union (map vars-of (args-of e)))
    :else #{}))

(defn eval-expr
  "Вычислить выражение в окружении env (после desugar)."
  [expr env]
  (let [e (desugar expr)]
    (cond
      (boolean? e) e
      (symbol?  e) (boolean (env e))
      (op-node? e)
      (case (op-of e)
        :not (not (eval-expr (first (args-of e)) env))
        :and (every? true? (map #(eval-expr % env) (args-of e)))
        :or  (boolean (some true? (map #(eval-expr % env) (args-of e))))
        (throw (ex-info "Unexpected op in eval" {:op (op-of e)})))
      :else (throw (ex-info "Bad expr in eval" {:expr e})))))

(def ^:private prec
  "Приоритеты для печати (чем больше — тем выше)."
  {:lit 5 :not 4 :and 3 :or 2 :xor 1 :imp 0})

(defn- op->pp [op]
  (case op
    :not "!"
    :and " & "
    :or  " | "
    :xor " ^ "
    :imp " -> "
    "?"))

(defn pp
  "Вернуть инфиксную строку для выражения (минимальные скобки)."
  ([expr] (pp expr (:lit prec)))
  ([expr ctxp]
   (cond
     (true? expr)  "true"
     (false? expr) "false"
     (symbol? expr) (name expr)
     (op-node? expr)
     (let [op (op-of expr) as (args-of expr)]
       (case op
         :not (let [s (pp (first as) (:not prec))]
                (str "!" (if (< (if (op-node? (first as)) (prec (op-of (first as))) (:lit prec))
                                (:not prec))
                           (str "(" s ")")
                           s)))
         :and (let [p (:and prec)
                    parts (map #(let [s (pp % p)
                                      subp (if (op-node? %) (prec (op-of %)) (:lit prec))]
                                  (if (< subp p) (str "(" s ")") s))
                               as)
                    s (str/join (op->pp :and) parts)]
                (if (< (:and prec) ctxp) (str "(" s ")") s))
         :or  (let [p (:or prec)
                    parts (map #(let [s (pp % p)
                                      subp (if (op-node? %) (prec (op-of %)) (:lit prec))]
                                  (if (< subp p) (str "(" s ")") s))
                               as)
                    s (str/join (op->pp :or) parts)]
                (if (< (:or prec) ctxp) (str "(" s ")") s))
         :xor (let [lhs (first as) rhs (second as)
                    p (:xor prec)
                    L (pp lhs p) R (pp rhs (dec p))
                    Lp (if (op-node? lhs) (prec (op-of lhs)) (:lit prec))
                    Rp (if (op-node? rhs) (prec (op-of rhs)) (:lit prec))
                    Ls (if (< Lp p) (str "(" L ")") L)
                    Rs (if (<= Rp (dec p)) (str "(" R ")") R)
                    s (str Ls (op->pp :xor) Rs)]
                (if (< p ctxp) (str "(" s ")") s))
         :imp (let [lhs (first as) rhs (second as)
                    p (:imp prec)
                    L (pp lhs (inc p)) R (pp rhs p)
                    s (str L (op->pp :imp) R)]
                (if (< p ctxp) (str "(" s ")") s))
         :twoof3 (pp (desugar expr) ctxp)
         :nor    (pp (desugar expr) ctxp)
         :nand   (pp (desugar expr) ctxp)
         (pp (desugar expr) ctxp)))
     :else (str expr))))

(defn dnf-str
  "Вернуть строку ДНФ для expr."
  [expr]
  (pp (to-dnf expr)))

(defn parse
  "Разобрать инфиксную строку s в наше AST.
   Поддерживается: !, &, |, ^, ->, скобки, идентификаторы A,B,C..., а также вызовы:
   and(...), or(...), not(x), xor(a,b), nor(a,b), nand(a,b), twoof3(a,b,c)."
  [^String s]
  (let [len (.length s)
        idx (atom 0)
        peekc (fn [] (when (< @idx len) (.charAt s @idx)))
        skip-ws (fn [] (while (and (< @idx len)
                                   (Character/isWhitespace (.charAt s @idx)))
                         (swap! idx inc)))
        accept (fn [^String t]
                 (skip-ws)
                 (let [i @idx L (.length t)]
                   (if (and (<= (+ i L) len)
                            (= (.substring s i (+ i L)) t))
                     (do (swap! idx #(+ % L)) true)
                     false)))
        parse-ident (fn []
                      (skip-ws)
                      (let [i @idx]
                        (when (and (< @idx len)
                                   (let [c (.charAt s @idx)]
                                     (or (Character/isLetter c) (= \_ c))))
                          (swap! idx inc)
                          (while (and (< @idx len)
                                      (let [c (.charAt s @idx)]
                                        (or (Character/isLetterOrDigit c) (= \_ c))))
                            (swap! idx inc))
                          (.substring s i @idx))))]
    (letfn [(parse-expr [] (parse-imp))
            (parse-imp []
              (let [lhs (parse-xor)]
                (skip-ws)
                (if (accept "->")
                  (IMP lhs (parse-imp))
                  lhs)))
            (parse-xor []
              (loop [lhs (parse-or)]
                (skip-ws)
                (if (accept "^")
                  (recur (XOR lhs (parse-or)))
                  lhs)))
            (parse-or []
              (loop [lhs (parse-and)]
                (skip-ws)
                (if (accept "|")
                  (recur (OR lhs (parse-and)))
                  lhs)))
            (parse-and []
              (loop [lhs (parse-unary)]
                (skip-ws)
                (if (accept "&")
                  (recur (AND lhs (parse-unary)))
                  lhs)))
            (parse-unary []
              (skip-ws)
              (cond
                (accept "!") (NOT (parse-unary))
                (accept "(") (let [e (parse-expr)]
                               (when-not (accept ")")
                                 (throw (ex-info "Ожидалась закрывающая скобка ')'" {:pos @idx})))
                               e)
                :else (parse-primary)))
            (parse-args []
              (skip-ws)
              (let [args (transient [])]
                (if (accept ")")
                  (persistent! args)
                  (do
                    (conj! args (parse-expr))
                    (loop []
                      (skip-ws)
                      (cond
                        (accept ",") (do (conj! args (parse-expr))
                                         (recur))
                        (accept ")") (persistent! args)
                        :else (throw (ex-info "Ожидалась ',' или ')' в списке аргументов" {:pos @idx}))))))))
            (parse-primary []
              (skip-ws)
              (let [name (parse-ident)]
                (when-not name
                  (throw (ex-info "Ожидался идентификатор/переменная" {:pos @idx})))
                (skip-ws)
                (if (accept "(")
                  (let [args (parse-args)
                        lname (str/lower-case name)]
                    (case lname
                      "not"   (NOT (first args))
                      "and"   (apply AND  args)
                      "or"    (apply OR   args)
                      "xor"   (apply XOR  args)
                      "nor"   (apply NOR  args)
                      "nand"  (apply NAND args)
                      "imp"   (apply IMP  args)
                      "twoof3"(let [[a b c] args] (TWOOF3 a b c))
                      (throw (ex-info (str "Неизвестная функция: " lname) {:name lname}))))
                  (case (str/lower-case name)
                    "true" true
                    "false" false
                    (symbol name)))))]
      (let [res (parse-expr)]
        (skip-ws)
        (when (< @idx len)
          (throw (ex-info "Лишние символы в конце ввода" {:pos @idx})))
        res))))

(defn -main
  "Интерактивный запуск: ввод инфиксной строки → печать ДНФ."
  [& _args]
  (println "Введите булево выражение.")
  (println "Операторы: !  &  |  ^  ->   и скобки (). Функции: twoof3(a,b,c), xor(a,b), nor(a,b), nand(a,b).")
  (print   "Ввод> ") (flush)
  (if-let [line (read-line)]
    (try
      (let [expr (parse line)
            dnf  (to-dnf expr)]
        (println "AST        :" expr)
        (println "DNF (AST)  :" dnf)
        (println "DNF (str)  :" (dnf-str expr)))
      (catch Exception e
        (println "Ошибка:" (.getMessage e))))
    (println "Пустой ввод.")))

;; INPUT TESTIKI:
;; $cases=@('A & (B | C)','A -> B','xor(A,B)','A ^ B','nor(A,B)','nand(A,B)','twoof3(A,B,C)','!(A | B) & C','true | A & false','A & B -> C','A | B ^ C','foo_1 & !bar | baz2','xnor(A,B)'); foreach($q in $cases){Write-Host ''; Write-Host "=== INPUT: $q ==="; $q | clojure -M -m lab4_dnf}
