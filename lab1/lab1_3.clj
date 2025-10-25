(ns lab1_3)

(defn my-map
  "Аналог map"
  [f coll]
  (reverse
    (reduce (fn [acc x]
              (cons (f x) acc))
            '()
            coll)))

(defn my-filter
  "Аналог filter"
  [pred coll]
  (reverse
    (reduce (fn [acc x]
              (if (pred x)
                (cons x acc)
                acc))
            '()
            coll)))

(defn -main []
  (println (my-map inc  [1 2 3 4]))
  (println (my-map #(* % %) '(1 2 3 4)))
  (println (my-map str [\a \b \c]))

  (println (my-filter odd?  [1 2 3 4 5]))
  (println (my-filter even? '(1 2 3 4 5)))
  (println (my-filter #(<= % 3) [1 2 3 4]))

  (println (my-map identity nil))
  (println (my-filter even?  nil))
  (println (my-map inc  '()))
  (println (my-filter odd? '())))

(-main)
