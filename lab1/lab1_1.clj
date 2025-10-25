(ns lab1_1)

(defn combos [alphabet n]
  (cond
    (neg? n) '()
    (zero? n) '("")                                  
    :else (for [p  (combos alphabet (dec n))
                ch alphabet
                :when (or (empty? p) (not= (last p) ch))]
            (str p ch))))

(defn -main []
  ;; (println (combos [\a \b \c] 2))
  ;; (println (combos [\a \b \c] 3))
  ;; (println (combos [\a \b \c] 4))
  ;; (println (combos [\a \b \c] 0))
  ;; (println (combos [\a] 1))
  (println (combos [\a \b] 10))
  )

(-main)
