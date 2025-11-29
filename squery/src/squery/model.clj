(ns squery.model
  "Представление документов в виде S-выражений и базовые операции над деревом."
  (:require [clojure.zip :as zip]))

(defn element?
  "Проверяет, что значение является узлом-дескриптором: последовательность, первый
   элемент – keyword (тег)."
  [x]
  (and (sequential? x)
       (keyword? (first x))))

(defn element-tag
  "Возвращает тег узла (keyword)."
  [node]
  (first node))

(defn element-attrs
  "Возвращает карту атрибутов узла. Если вторая позиция – карта, она считается
   атрибутами, иначе возвращается пустая карта."
  [node]
  (let [second-el (second node)]
    (if (map? second-el)
      second-el
      {})))

(defn element-children
  "Возвращает вектор детей узла (подузлы и скалярные значения)."
  [node]
  (let [second-el (second node)]
    (if (map? second-el)
      (vec (drop 2 node))
      (vec (drop 1 node)))))

(defn make-element
  "Создаёт узел из тега, карты атрибутов (может быть nil) и последовательности детей."
  [tag attrs children]
  (let [attrs (or attrs {})]
    (if (empty? attrs)
      (into [tag] children)
      (into [tag attrs] children))))

(defn map-children
  "Применяет функцию f к каждому ребёнку узла и возвращает новый узел."
  [node f]
  (let [children (element-children node)
        new-children (mapv (fn [ch]
                             (if (element? ch)
                               (f ch)
                               ch))
                           children)]
    (make-element (element-tag node)
                  (element-attrs node)
                  new-children)))


(defn branch?
  "Используется tree-seq/zipper: узел является ветвью, если это element?."
  [node]
  (element? node))

(defn children
  "Функция children для tree-seq/zipper."
  [node]
  (element-children node))

(defn make-node
  "Функция make-node для zip/zipper."
  [node children]
  (make-element (element-tag node)
                (element-attrs node)
                (vec children)))

(defn s-zipper
  "Создаёт zipper над документом."
  [root]
  (zip/zipper branch? children make-node root))

(defn element-text
  "Возвращает все текстовые (не element?) дети узла в одном строковом значении (конкатенация)."
  [node]
  (->> (element-children node)
       (remove element?)
       (map str)
       (apply str)))

(def sample-document
  [:catalog {:lang "en"}
   [:book {:id 1 :year 2020}
    [:title "Functional Programming in Clojure"]
    [:author "John Doe"]]
   [:book {:id 2 :year 2021}
    [:title "Practical Clojure"]
    [:author "Jane Roe"]]])
