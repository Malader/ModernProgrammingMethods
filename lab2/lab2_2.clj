(ns lab2_2)

(defn integral-trap-lazy
  [f h]
  (let [h (double h)]
    (when (<= h 0) (throw (ex-info "step h must be > 0" {:h h})))
    (let [xs    (iterate #(+ % h) 0.0)
          gs    (map (fn [x] (double (f x))) xs)
          areas (map (fn [[g0 g1]] (* 0.5 h (+ g0 g1)))
                      (partition 2 1 gs))
          S     (cons 0.0 (reductions + areas))]
      (fn [x]
        (let [sign (if (neg? x) -1.0 1.0)
              ax   (Math/abs (double x))
              n    (long (Math/floor (/ ax h)))
              r    (- ax (* n h))
              full (nth S n)
              tail (if (pos? r)
                     (let [a (* n h) b (+ a r)]
                       (* 0.5 r (+ (f a) (f b))))
                     0.0)]
          (* sign (+ full tail)))))))

(defn integral-trap-lazy-memo
  [f h]
  (memoize (integral-trap-lazy f h)))

(defn measure-ms
  [thunk]
  (let [t0 (System/nanoTime)
        v  (thunk)
        t1 (System/nanoTime)]
    {:result v :ms (/ (- t1 t0) 1e6)}))

(defn bench
  [F xs]
  (reduce (fn [{:keys [total-ms rows]} x]
            (let [{:keys [result ms]} (measure-ms #(F x))]
              {:total-ms (+ total-ms ms)
               :rows     (conj rows {:x x :value result :ms ms})}))
          {:total-ms 0.0 :rows []}
          xs))

(defn print-bench
  [label h xs {:keys [total-ms]}]
  (let [cnt  (count xs)
        avg  (if (pos? cnt) (/ total-ms cnt) 0.0)
        xmin (apply min xs)
        xmax (apply max xs)]
    (println (format "%s | вызовов: %d | h=%.6f | min x=%.4f | max x=%.4f | total=%.3f ms | avg=%.3f ms"
                     label cnt h xmin xmax total-ms avg))))

(defn build-grid
  [xmax n]
  (let [n (long n)]
    (vec (for [i (range n)]
           (* xmax (/ i (double (dec n))))))))

(defn build-down-up-seq
  [x0 x1 x2 repeats]
  (let [triplet [x0 x1 x2]]
    (vec (map double (take (* 3 repeats) (cycle triplet))))))

(defn -main []
  (let [h           0.001
        xmax        10.0
        points      200
        xs          (build-grid xmax points)
        xs-shuffled (vec (shuffle xs))
        x0   7.5
        x1   3.0
        x2   9.0
        reps 200
        xs-down-up (build-down-up-seq x0 x1 x2 reps)

        f1 (fn [t] (* t t))
        f2 (fn [t] (Math/sin t))

        F1  (integral-trap-lazy f1 h)
        F2  (integral-trap-lazy f2 h)
        F1m (integral-trap-lazy-memo f1 h)
        F2m (integral-trap-lazy-memo f2 h)]

    (println "ПРОВЕРКА ТОЧНОСТИ (ленивый вариант):")
    (println "F1(2)  ≈" (F1 2.0)  " (точно 8/3 ≈ 2.666666...)")
    (println "F1(-2) ≈" (F1 -2.0) " (должно быть -8/3)")
    (println "F2(pi) ≈" (F2 Math/PI) " (точно 2)")

    (println)
    (println "БЕНЧМАРК (ленивый): точки возрастают (лучший случай)")
    (print-bench "lazy F1"  h xs (bench F1 xs))
    (print-bench "lazy F1m" h xs (bench F1m xs))
    (print-bench "lazy F2"  h xs (bench F2 xs))
    (print-bench "lazy F2m" h xs (bench F2m xs))

    (println)
    (println "БЕНЧМАРК (ленивый): точки перемешаны")
    (print-bench "lazy F1"  h xs-shuffled (bench F1 xs-shuffled))
    (print-bench "lazy F1m" h xs-shuffled (bench F1m xs-shuffled))
    (print-bench "lazy F2"  h xs-shuffled (bench F2 xs-shuffled))
    (print-bench "lazy F2m" h xs-shuffled (bench F2m xs-shuffled))

    (println)
    (println "БЕНЧМАРК (ленивый): сценарий уменьшения/увеличения [x0, x1<x0, x2>x0] * repeats")
    (print-bench (format "lazy F1  (x0=%.2f, x1=%.2f, x2=%.2f)" x0 x1 x2) h xs-down-up (bench F1  xs-down-up))
    (print-bench (format "lazy F1m (x0=%.2f, x1=%.2f, x2=%.2f)" x0 x1 x2) h xs-down-up (bench F1m xs-down-up))
    (print-bench (format "lazy F2  (x0=%.2f, x1=%.2f, x2=%.2f)" x0 x1 x2) h xs-down-up (bench F2  xs-down-up))
    (print-bench (format "lazy F2m (x0=%.2f, x1=%.2f, x2=%.2f)" x0 x1 x2) h xs-down-up (bench F2m xs-down-up)) ))

(-main)
