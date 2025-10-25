(ns lab2_1_timer)

(defn integral-trap
  "Возвращает F(x) ≈ ∫_0^x f(t) dt по методу трапеций с шагом h."
  [f h]
  (let [h (double h)]
    (when (<= h 0) (throw (ex-info "step h must be > 0" {:h h})))
    (fn [x]
      (let [sign (if (neg? x) -1.0 1.0)
            ax   (Math/abs (double x))
            n    (long (Math/floor (/ ax h)))
            r    (- ax (* n h))
            full (reduce (fn [s k]
                           (let [a (* k h) b (+ a h)]
                             (+ s (* 0.5 h (+ (f a) (f b))))))
                         0.0
                         (range n))
            tail (if (pos? r)
                   (let [a (* n h) b (+ a r)]
                     (* 0.5 r (+ (f a) (f b))))
                   0.0)]
        (* sign (+ full tail))))))

(defn integral-trap-memo
  [f h]
  (let [h   (double h)
        _   (when (<= h 0) (throw (ex-info "step h must be > 0" {:h h})))
        f*  (memoize (fn [x] (double (f x))))
        area (memoize (fn [k]
                        (let [a (* k h) b (+ a h)]
                          (* 0.5 h (+ (f* a) (f* b))))))]
    (fn [x]
      (let [sign (if (neg? x) -1.0 1.0)
            ax   (Math/abs (double x))
            n    (long (Math/floor (/ ax h)))
            r    (- ax (* n h))
            full (loop [k 0, s 0.0]
                   (if (< k n)
                     (recur (inc k) (+ s (area k)))
                     s))
            tail (if (pos? r)
                   (let [a (* n h) b (+ a r)]
                     (* 0.5 r (+ (f* a) (f* b))))
                   0.0)]
        (* sign (+ full tail))))))

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
        xmax (apply max xs)]
    (println (format "%s | вызовов: %d | h=%.6f | max x=%.4f | total=%.3f ms | avg=%.3f ms"
                     label cnt h xmax total-ms avg))))

(defn build-grid
  [xmax n]
  (let [n (long n)]
    (vec (for [i (range n)]
           (* xmax (/ i (double (dec n))))))))

(defn -main []
  (let [h           0.001
        xmax        10.0
        points      200
        xs          (build-grid xmax points)
        xs-shuffled (vec (shuffle xs))

        f1  (fn [t] (* t t))
        f2  (fn [t] (Math/sin t))

        F1  (integral-trap      f1 h)
        F1m (integral-trap-memo f1 h)
        F2  (integral-trap      f2 h)
        F2m (integral-trap-memo f2 h)]

    (println "ПРОВЕРКА ТОЧНОСТИ:")
    (println "int_0^2 t^2 dt ~" (F1 2.0)  " (точно 8/3 ≈ 2.666666...)")
    (println "int_0^2 t^2 dt (memo) ~" (F1m 2.0))
    (println "int_0^-2 t^2 dt ~" (F1 -2.0) " (должно быть -8/3)")
    (println "int_0^pi sin t dt ~" (F2 Math/PI) " (точно 2)")
    (println "int_0^pi sin t dt (memo) ~" (F2m Math/PI))

    (println)
    (println "БЕНЧМАРК: f1(t)=t^2, точки возрастают")
    (print-bench "без кэша (F1)" h xs (bench F1 xs))
    (print-bench "мемо (F1m)"    h xs (bench F1m xs))

    (println)
    (println "БЕНЧМАРК: f1(t)=t^2, точки перемешаны")
    (print-bench "без кэша (F1)" h xs-shuffled (bench F1 xs-shuffled))
    (print-bench "мемо (F1m)"    h xs-shuffled (bench F1m xs-shuffled))

    (println)
    (println "БЕНЧМАРК: f2(t)=sin t, точки возрастают")
    (print-bench "без кэша (F2)" h xs (bench F2 xs))
    (print-bench "мемо (F2m)"    h xs (bench F2m xs))))

(-main)
