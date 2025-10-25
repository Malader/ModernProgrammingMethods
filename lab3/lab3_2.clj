(ns lab3_2)

(defn chunks-of [coll n]
  (when (<= n 0) (throw (ex-info "block-size must be > 0" {:n n})))
  (letfn [(step [xs]
            (lazy-seq
              (when-let [s (seq xs)]
                (let [chunk (doall (take n s))
                      rest  (drop n s)]
                  (cons chunk (step rest))))))]
    (step coll)))

(defn pfilter-lazy
  ([pred coll block-size]
   (pfilter-lazy pred coll block-size (.availableProcessors (Runtime/getRuntime))))
  ([pred coll block-size parallelism]
   (let [n  (max 1 (long block-size))
         p  (max 1 (long parallelism))
         chs (chunks-of coll n)
         process (fn [ch] (future (doall (filter pred ch))))]
     (letfn [(fill [futs cs]
               (loop [f futs, c cs]
                 (if (and (< (count f) p) (seq c))
                   (recur (conj f (process (first c))) (rest c))
                   [f c])))
             (produce [cs futs]
               (lazy-seq
                 (let [[f2 c2] (fill futs cs)]
                   (when (seq f2)
                     (let [batch @(first f2)]
                       (concat batch (produce c2 (subvec f2 1))))))))]
       (produce chs [])))))

(defn measure-ms [thunk]
  (let [t0 (System/nanoTime)
        v  (thunk)
        t1 (System/nanoTime)]
    {:ms (/ (- t1 t0) 1e6) :value v}))

(defn prime? [n]
  (cond
    (< n 2) false
    (= n 2) true
    (even? n) false
    :else (let [lim (long (Math/floor (Math/sqrt (double n))))]
            (loop [d 3]
              (cond
                (> d lim) true
                (zero? (mod n d)) false
                :else (recur (+ d 2)))))))

(defn bench-take-finite [data sizes block-size par]
  (println "=== take N на конечном массиве ===")
  (println "len=" (count data) "block=" block-size "par=" par)
  (doseq [n sizes]
    (let [seqN (measure-ms #(vec (doall (take n (filter prime? data)))))
          parN (measure-ms #(vec (doall (take n (pfilter-lazy prime? data block-size par)))))
          ok   (= (:value seqN) (:value parN))
          spd  (let [a (:ms seqN) b (:ms parN)]
                 (if (pos? b) (/ a b) Double/NaN))]
      (println "N=" n
               "| seq_ms=" (double (:ms seqN))
               "| par_ms=" (double (:ms parN))
               "| speedup=" (double spd)
               "| ok=" ok))))

(defn bench-take-infinite [sizes block-size par]
  (println "=== take N на бесконечном потоке ===")
  (println "src=iterate inc 1" "block=" block-size "par=" par)
  (doseq [n sizes]
    (let [seqN (measure-ms #(vec (doall (take n (filter prime? (iterate inc 1))))))
          parN (measure-ms #(vec (doall (take n (pfilter-lazy prime? (iterate inc 1) block-size par)))))
          ok   (= (:value seqN) (:value parN))
          spd  (let [a (:ms seqN) b (:ms parN)]
                 (if (pos? b) (/ a b) Double/NaN))]
      (println "N=" n
               "| seq_ms=" (double (:ms seqN))
               "| par_ms=" (double (:ms parN))
               "| speedup=" (double spd)
               "| ok=" ok
               "| first10=" (pr-str (take 10 (:value parN)))))))

(defn bench-full [data block-size par]
  (println "=== полный прогон на конечном массиве ===")
  (println "len=" (count data) "block=" block-size "par=" par)
  (let [seq-all (measure-ms #(vec (doall (filter prime? data))))
        par-all (measure-ms #(vec (doall (pfilter-lazy prime? data block-size par))))
        ok      (= (:value seq-all) (:value par-all))
        spd     (let [a (:ms seq-all) b (:ms par-all)]
                  (if (pos? b) (/ a b) Double/NaN))]
    (println "FULL | seq_ms=" (double (:ms seq-all))
             "| par_ms=" (double (:ms par-all))
             "| speedup=" (double spd)
             "| ok=" ok
             "| count=" (count (:value par-all)))))

(defn -main []
  (try
    (let [data       (vec (range 1 500000))
          block-size 4096
          par        (max 2 (.availableProcessors (Runtime/getRuntime)))
          sizes      [1000 2000 3000]]
      (bench-take-finite  data sizes block-size par)
      (bench-take-infinite      sizes block-size par)
      (bench-full         data        block-size par))
    (finally
      (shutdown-agents))))

(-main)
