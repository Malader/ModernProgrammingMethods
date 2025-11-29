(ns squery.query-test
  (:require [clojure.test :refer :all]
            [squery.model :as m]
            [squery.path :as path]
            [squery.query :as q]))

(deftest select-relative-basic
  (let [books (q/select m/sample-document [:book])]
    (is (= 2 (count books)))))

(deftest select-with-condition
  (let [p (path/relative [:book {:attrs {:id 1}}])
        books (q/select m/sample-document p)]
    (is (= 1 (count books)))
    (is (= 1 (-> books first (m/element-attrs) :id)))))

(deftest select-any-depth
  (let [p (path/relative path/any-depth {:tag :author})
        authors (q/select m/sample-document p)]
    (is (= 2 (count authors)))))

(deftest update-year
  (let [p (path/relative [:book {}])
        new-doc (q/update* m/sample-document p
                           (fn [node]
                             (let [attrs (m/element-attrs node)]
                               (m/make-element (m/element-tag node)
                                               (update attrs :year inc)
                                               (m/element-children node)))))
        years (->> (q/select new-doc [:book])
                   (map (comp :year m/element-attrs)))]
    (is (= [2021 2022] (vec years)))))
