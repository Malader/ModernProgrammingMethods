(ns squery.model-test
  (:require [clojure.test :refer :all]
            [squery.model :as m]))

(deftest element-basic-test
  (is (m/element? [:a]))
  (is (m/element? [:a {:id 1}]))
  (is (not (m/element? {:a 1})))
  (is (= :catalog (m/element-tag m/sample-document)))
  (is (= {:lang "en"} (m/element-attrs m/sample-document)))
  (is (= 2 (count (m/element-children m/sample-document)))))
