(ns lab1_2)

(defn combos-tail [alphabet n]
  (cond
    (neg? n)        '()
    (zero? n)       '("")
    (empty? alphabet) '()
    :else
    (loop [k   n
           acc '("")]
      (if (zero? k)
        acc
        (recur (dec k)
               (mapcat (fn [p]
                         (for [ch alphabet
                               :when (or (empty? p)
                                         (not= (last p) ch))]
                           (str p ch)))
                       acc))))))

(defn -main []
  (println (combos-tail [\a \b \c] 2))
  (println (combos-tail [\a \b \c] 3))
  (println (combos-tail [\a] 1))
  (println (combos-tail [] 3))
  (println (combos-tail [\x \y] 0)))

(-main)
