(ns squery.core
  "Примеры использования библиотеки squery."
  (:gen-class)
  (:require [squery.model :as m]
            [squery.path :as path]
            [squery.query :as q]
            [squery.schema :as sch]
            [squery.html :as html]))

(defn demo-query []
  (println "\nOriginal document:")
  (prn m/sample-document)

  (println "\nBooks (relative path [:book]):")
  (doseq [n (q/select m/sample-document [:book])]
    (prn n))

  (println "\nSearch all books title (absolute path [:book :title]):")
  (doseq [n (q/select m/sample-document [:book :title])]
    (prn n))

  (println "\nBooks with id=1 (path with attribute condition):")
  (let [p (path/relative [:book {:attrs {:id 1}}])]
    (doseq [n (q/select m/sample-document p)]
      (prn n)))

  (println "\nAuthors anywhere (:** segment):")
  (let [p (path/relative path/any-depth {:tag :author})]
    (doseq [n (q/select m/sample-document p)]
      (prn n))))

(defn demo-update []
  (println "\nDocument after year increment:")
  (let [p (path/relative [:book {}])
        new-doc (q/update* m/sample-document p
                           (fn [node]
                             (let [attrs (m/element-attrs node)
                                   year  (:year attrs)]
                               (m/make-element (m/element-tag node)
                                               (assoc attrs :year (inc year))
                                               (m/element-children node)))))]
    (prn new-doc)))

(defn demo-schema []
  (println "\nSchema validation result:")
  (let [{:keys [ok? errors]} (sch/validate sch/sample-schema m/sample-document)]
    (println "ok? =" ok?)
    (doseq [e errors]
      (prn e))))

(defn demo-html []
  (println "\nTransform to HTML:")
  (let [html-str (html/transform-document m/sample-document html/sample-rules)]
    (println html-str)))

(defn -main
  [& _args]
  (demo-query)
  (demo-update)
  (demo-schema)
  (demo-html))
