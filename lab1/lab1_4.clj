(ns lab1_4_2)

(defn extend-one
  [alphabet s]
  (let [allowed (if (empty? s)
                  alphabet
                  (filter #(not= (last s) %) alphabet))]
    (map #(str s %) allowed)))

(defn extend-all
  [alphabet acc]
  (loop [xs  acc
         res []]
    (if (empty? xs)
      res
      (let [p    (first xs)
            exts (extend-one alphabet p)]
        (recur (rest xs) (into res exts))))))

(defn combos-tail
  [alphabet n]
  (cond
    (neg? n)          '()
    (zero? n)         '("")
    (empty? alphabet) '()
    :else
    (loop [step n
           acc  '("")]
      (if (zero? step)
        (apply list acc)
        (recur (dec step) (extend-all alphabet acc))))))


(defn bench
  [label f & args]
  (let [t0 (System/nanoTime)
        ret (apply f args)
        t1 (System/nanoTime)
        ms (/ (double (- t1 t0)) 1.0e6)]
    (println label "— время:" (format "%.3f мс" ms) ", количество:" (count ret))
    ret))


(defn -main []
  (println (combos-tail [\a \b \c] 2))
  (println (combos-tail [\a \b \c] 3))
  (println (combos-tail [\a] 3))
  (println (combos-tail [] 3))
  (println (combos-tail [\x \y] 0))

  (println "— БЕНЧМАРК TAIL —")
  (bench "TAIL [a b c], n=2" combos-tail [\a \b \c] 2)
  (bench "TAIL [a b c], n=3" combos-tail [\a \b \c] 3)
  (bench "TAIL [x y],   n=7" combos-tail [\x \y] 7))

(-main)
