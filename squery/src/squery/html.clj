(ns squery.html
  "Упрощённый язык трансформации документа в HTML (аналог XSLT, но сильно упрощённый)."
  (:require [clojure.string :as str]
            [squery.model :as m]))

(defn- rule-for
  [rules tag]
  (or (get (:rules rules) tag)
      {:tag (:default-tag rules :div)}))

(defn transform-node
  "Преобразует один узел документа в HTML-узел (hiccup-представление)
   согласно правилам rules."
  [rules node]
  (let [tag       (m/element-tag node)
        attrs     (m/element-attrs node)
        children  (m/element-children node)
        {:keys [tag class]} (rule-for rules tag)
        base-attrs (cond-> {}
                     class (assoc :class class))
        data-attrs (into {}
                         (map (fn [[k v]]
                                [(keyword (str "data-" (name k))) (str v)]))
                         attrs)
        html-children (map (fn [ch]
                             (if (m/element? ch)
                               (transform-node rules ch)
                               (str ch)))
                           children)]
    (into [tag (merge base-attrs data-attrs)] html-children)))

(defn render-html
  "Рендерит hiccup-представление в строку HTML.
   Реализовано минимально, без экранирования спецсимволов."
  [node]
  (cond
    (string? node) node
    (sequential? node)
    (let [[tag attrs & children] node
          attrs-str (->> attrs
                         (map (fn [[k v]]
                                (format " %s=\"%s\"" (name k) (str v))))
                         (apply str))
          inner (->> children
                     (map render-html)
                     (apply str))]
      (format "<%s%s>%s</%s>" (name tag) attrs-str inner (name tag)))
    :else (str node)))

(defn transform-document
  "Трансформирует S-выражение документа root в строку HTML.
   rules — карта с описанием правил (см. rule-for)."
  [root rules]
  (-> (transform-node rules root)
      (render-html)))

(def sample-rules
  {:default-tag :div
   :rules {:catalog {:tag :div :class "catalog"}
           :book    {:tag :article :class "book"}
           :title   {:tag :h2 :class "title"}
           :author  {:tag :p :class "author"}}})
