(ns lab4_dnf_test
  (:require [clojure.test :refer :all]
            [lab4_dnf :as d]))

(defn assignments [vars]
  (let [vs (vec vars) n (count vs)]
    (map (fn [mask]
           (into {} (map-indexed (fn [i v] [v (bit-test mask i)]) vs)))
         (range (bit-shift-left 1 n)))))

(defn equivalent? [expr1 expr2]
  (let [vars (d/vars-of (d/OR expr1 expr2))]
    (every? (fn [env] (= (d/eval-expr expr1 env)
                         (d/eval-expr expr2 env)))
            (assignments vars))))

(deftest basics-and-imp
  (let [A (d/VAR 'A) B (d/VAR 'B) C (d/VAR 'C)]
    (is (= (d/to-dnf (d/AND A (d/OR B C)))
           (d/OR (d/AND A B) (d/AND A C))))
    (is (= (d/to-dnf (d/IMP A B))
           (d/OR (d/NOT A) B)))
    (is (d/dnf? (d/to-dnf (d/AND A (d/OR B C)))))))

(deftest derived-ops-and-extensibility
  (let [A (d/VAR 'A) B (d/VAR 'B)]
    (is (= (d/to-dnf (d/XOR A B))
           (d/OR (d/AND A (d/NOT B)) (d/AND (d/NOT A) B))))
    (is (= (d/to-dnf (d/NOR A B))
           (d/AND (d/NOT A) (d/NOT B))))
    (is (= (d/to-dnf (d/NAND A B))
           (d/OR (d/NOT A) (d/NOT B))))
    (d/register-derived-op! :xnor (fn [[a b]] (d/OR (d/AND a b) (d/AND (d/NOT a) (d/NOT b)))))
    (is (= (d/to-dnf {:op :xnor :args [A B]})
           (d/OR (d/AND A B) (d/AND (d/NOT A) (d/NOT B)))))))

(deftest substitution
  (let [A (d/VAR 'A) B (d/VAR 'B)]
    (is (= (d/substitute->dnf (d/AND A (d/OR B (d/NOT A))) {'A true})
           B))
    (is (= (d/substitute->dnf (d/OR (d/NOT A) (d/AND A B)) {'A false})
           true))
    (is (= (d/substitute->dnf (d/AND (d/NOT A) (d/OR A B)) {'A true})
           false))))

(deftest constants-and-dup
  (let [A (d/VAR 'A)]
    (is (= (d/to-dnf (d/OR true A false))
           true))
    (is (= (d/to-dnf (d/AND true A true))
           A))
    (is (= (d/to-dnf (d/AND A A))
           A))
    (is (= (d/to-dnf (d/OR A A))
           A))))

(deftest equivalence-and-idempotence
  (let [A (d/VAR 'A) B (d/VAR 'B) C (d/VAR 'C)
        expr (d/AND (d/OR A (d/NOT B))
                    (d/OR (d/NOT A) C)
                    (d/OR B C))
        dnf1 (d/to-dnf expr)
        dnf2 (d/to-dnf dnf1)]
    (is (d/dnf? dnf1))
    (is (equivalent? expr dnf1))
    (is (= dnf1 dnf2))))


(deftest parse-precedence-and-assoc
  (let [A (d/VAR 'A) B (d/VAR 'B) C (d/VAR 'C)]
    (is (= (d/parse "A & (B | C)")
           (d/AND A (d/OR B C))))
    (is (= (d/parse "A | B ^ C")
           (d/XOR (d/OR A B) C)))
    (is (= (d/parse "A -> B -> C")
           (d/IMP A (d/IMP B C))))
    (is (= (d/parse "!A & B")
           (d/AND (d/NOT A) B)))
    (is (= (d/parse "xor(A,B)")
           (d/XOR A B)))
    (is (= (d/parse "twoof3(A,B,C)")
           (d/TWOOF3 A B C)))))

(deftest dnf-str-output
  (is (= (d/dnf-str (d/parse "A & (B | C)"))
         "(A & B | A & C)"))
  (is (= (d/dnf-str (d/parse "A -> B"))
         "(!A | B)"))
  (is (= (d/dnf-str (d/parse "!(A | B) & C"))
         "(!A & !B & C)"))
  (is (= (d/dnf-str (d/parse "nand(A,B)"))
         "(!A | !B)"))
  (is (= (d/dnf-str (d/parse "A | B ^ C"))
         "(A & !C | B & !C | !A & !B & C)"))
  (is (= (d/dnf-str (d/parse "true | A & false"))
         "true"))
  (is (= (d/dnf-str (d/parse "twoof3(A,B,C)"))
         "(A & B & !C | A & !B & C | !A & B & C)")))

(deftest twoof3-semantics
  (let [A (d/VAR 'A) B (d/VAR 'B) C (d/VAR 'C)
        expr (d/TWOOF3 A B C)
        lowered (d/OR (d/AND A B (d/NOT C))
                      (d/AND A C (d/NOT B))
                      (d/AND B C (d/NOT A)))]
    (is (true?  (d/eval-expr expr {'A true  'B true  'C false})))
    (is (true?  (d/eval-expr expr {'A true  'B false 'C true })))
    (is (true?  (d/eval-expr expr {'A false 'B true  'C true })))
    (is (false? (d/eval-expr expr {'A true  'B true  'C true })))
    (is (false? (d/eval-expr expr {'A false 'B false 'C false})))
    (is (equivalent? expr lowered))
    (is (d/dnf? (d/to-dnf expr)))
    (is (equivalent? expr (d/to-dnf expr)))))

(deftest parse-substitution-integration
  (is (= (d/substitute->dnf (d/parse "A & (B | !A)") {'A true})
         'B))
  (is (= (d/substitute->dnf (d/parse "A & (B | !A)") {'A false})
         false))
  (is (= (d/substitute->dnf (d/parse "A -> B") {'A true})
         'B)))

(deftest vars-and-eval-on-parsed
  (let [expr (d/parse "twoof3(A,B,C) & (A | !C)")]
    (is (= (d/vars-of expr) #{'A 'B 'C}))
    (is (true?  (d/eval-expr expr {'A true  'B true  'C false})))
    (is (false? (d/eval-expr expr {'A false 'B true  'C false})))))

(deftest parse-errors
  (is (thrown-with-msg? Exception #"Неизвестная функция"
        (d/parse "xnor(A,B)")))
  (is (thrown-with-msg? Exception #"закрывающая скобка"
        (d/parse "(A & B) | (C & (D | E)"))))

(defn -main []
  (let [res (run-tests 'lab4_dnf_test)]
    (shutdown-agents)
    (when (pos? (+ (:fail res) (:error res)))
      (System/exit 1))))
