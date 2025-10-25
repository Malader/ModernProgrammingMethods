(ns lab3_1)

(defn chunk-by [coll block-size]
  (let [s (seq coll)]
    (loop [rest s, acc []]
      (if (seq rest)
        (let [chunk (doall (take block-size rest))
              rest' (drop block-size rest)]
          (recur rest' (conj acc chunk)))
        acc))))

(defn pfilter [pred coll block-size]
  (let [chunks (chunk-by coll block-size)
        futs   (mapv (fn [ch] (future (doall (filter pred ch)))) chunks)]
    (->> futs (map deref) (apply concat) vec)))

(defn measure-ms [thunk]
  (let [t0 (System/nanoTime)
        v  (thunk)
        t1 (System/nanoTime)]
    {:ms (/ (- t1 t0) 1e6) :value v}))

(defn prime? [^long n]
  (cond
    (< n 2) false
    (= n 2) true
    (even? n) false
    :else (let [limit (long (Math/floor (Math/sqrt n)))]
            (loop [d 3]
              (cond
                (> d limit) true
                (zero? (mod n d)) false
                :else (recur (+ d 2)))))))

(defn -main []
  (try
    (let [data       (vec (range 1 300000))
          block-size 4096
          seq-res    (measure-ms #(vec (doall (filter prime? data))))
          par-res    (measure-ms #(pfilter prime? data block-size))
          ok?        (= (:value seq-res) (:value par-res))]
      (println "Длина входных данных:" (count data) " блок:" block-size)
      (println "Результаты совпадают? ->" ok?)
      (println "Обычный filter, мс:" (:ms seq-res) "   Кол-во простых:" (count (:value seq-res)))
      (println "Параллельный pfilter, мс:" (:ms par-res) "   Кол-во простых:" (count (:value par-res)))
      (when (pos? (:ms par-res))
        (println "Ускорение (seq/par):" (double (/ (:ms seq-res) (:ms par-res))))))
    (finally
      (shutdown-agents))))

(-main)
