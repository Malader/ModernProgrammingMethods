(ns squery.query
  "Поиск и модификация S-выражений по путям (аналог XPath)."
  (:require [squery.model :as m]
            [squery.path :as p]))

(defn- match-from-node
  "Ищет все узлы, которые соответствуют полному пути segs, начиная с узла node.
   Возвращает последовательность узлов-документов.
   Поддерживает сегмент переменной вложенности (:any-depth)."
  [node segs]
  (let [[seg & rest-segs] segs]
    (cond
      (nil? seg)
      [node]

      (p/any-depth-segment? seg)
      (let [children (m/element-children node)]
        (concat
         (when (seq rest-segs)
           (match-from-node node rest-segs))
         (mapcat #(when (m/element? %)
                    (match-from-node % segs))
                 children)))

      (p/segment-matches? seg node)
      (if (seq rest-segs)
        (mapcat #(when (m/element? %)
                   (match-from-node % rest-segs))
                (m/element-children node))
        [node])

      :else
      [])))

(defn- all-elements
  "Последовательность всех element?-узлов в дереве (включая корень)."
  [root]
  (tree-seq m/branch? m/children root))

(defn select
  "Выполняет поиск узлов по пути path в документе root.
   path — либо map {:kind :absolute|:relative, :segments [...]},
          либо просто последовательность сегментов (относительный путь).
   Возвращает последовательность найденных узлов (без дубликатов)."
  [root path]
  (let [{:keys [kind segments]} (p/normalize-path path)
        matches (case kind
                  :absolute (match-from-node root segments)
                  :relative (mapcat #(match-from-node % segments)
                                    (all-elements root)))]
    (distinct matches)))

(defn select-one
  "Возвращает первый найденный узел по пути path или nil."
  [root path]
  (first (select root path)))

(defn- transform-from-node
  "Рекурсивно трансформирует дерево, применяя f к узлам, которые
   удовлетворяют пути segs. Используется для относительных путей."
  [node segs f]
  (let [[seg & rest-segs] segs]
    (cond
      (nil? seg)
      (f node)

      (p/any-depth-segment? seg)
      (let [node-after-zero
            (if (seq rest-segs)
              (transform-from-node node rest-segs f)
              (f node))
            new-children
            (mapv (fn [ch]
                    (if (m/element? ch)
                      (transform-from-node ch segs f)
                      ch))
                  (m/element-children node-after-zero))]
        (m/make-element (m/element-tag node-after-zero)
                        (m/element-attrs node-after-zero)
                        new-children))

      (p/segment-matches? seg node)
      (if (seq rest-segs)
        (let [children     (m/element-children node)
              new-children (mapv (fn [ch]
                                   (if (m/element? ch)
                                     (transform-from-node ch rest-segs f)
                                     ch))
                                 children)]
          (m/make-element (m/element-tag node)
                          (m/element-attrs node)
                          new-children))
        (f node))

      :else
      (let [children     (m/element-children node)
            new-children (mapv (fn [ch]
                                 (if (m/element? ch)
                                   (transform-from-node ch segs f)
                                   ch))
                               children)]
        (m/make-element (m/element-tag node)
                        (m/element-attrs node)
                        new-children)))))

(defn- transform-absolute
  "Трансформация для абсолютного пути: путь должен начинаться с корня."
  [node segs f]
  (let [[seg & rest-segs] segs]
    (cond
      (nil? seg)
      (f node)

      (p/any-depth-segment? seg)
      (let [node-after-zero
            (if (seq rest-segs)
              (transform-absolute node rest-segs f)
              (f node))
            new-children
            (mapv (fn [ch]
                    (if (m/element? ch)
                      (transform-absolute ch segs f)
                      ch))
                  (m/element-children node-after-zero))]
        (m/make-element (m/element-tag node-after-zero)
                        (m/element-attrs node-after-zero)
                        new-children))

      (p/segment-matches? seg node)
      (if (seq rest-segs)
        (let [children     (m/element-children node)
              new-children (mapv (fn [ch]
                                   (if (m/element? ch)
                                     (transform-absolute ch rest-segs f)
                                     ch))
                                 children)]
          (m/make-element (m/element-tag node)
                          (m/element-attrs node)
                          new-children))
        (f node))

      :else
      node)))

(defn update*
  "Возвращает новый документ, в котором ко всем узлам, удовлетворяющим пути path,
   применена функция f.

   path — абсолютный или относительный путь (см. squery.path).
   f    — функция (node -> node').

   Примеры:
   (update* doc [:book] f)
   (update* doc (path/relative [:book {:attrs {:id 1}}]) f)"
  [root path f]
  (let [{:keys [kind segments]} (p/normalize-path path)]
    (case kind
      :absolute (transform-absolute root segments f)
      :relative (transform-from-node root segments f))))
