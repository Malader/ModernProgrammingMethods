(ns squery.schema-test
  (:require [clojure.test :refer :all]
            [squery.model :as m]
            [squery.schema :as sch]))

(deftest valid-sample-document
  (let [{:keys [ok? errors]} (sch/validate sch/sample-schema m/sample-document)]
    (is ok?)
    (is (empty? errors))))

(deftest invalid-missing-attr
  (let [bad-doc
        [:catalog {:lang "en"}
         [:book {:year 2020}
          [:title "x"]
          [:author "y"]]]
        {:keys [ok? errors]} (sch/validate sch/sample-schema bad-doc)]
    (is (not ok?))
    (is (some #(= (:kind %) :missing-attribute) errors))))
