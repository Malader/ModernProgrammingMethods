(ns squery.schema
  "Представление схемы (аналог XML Schema) и проверка документов по схеме."
  (:require [squery.model :as m]))


(defn value-of-type?
  "Проверяет, соответствует ли значение v типу t (:string, :int, :number, :boolean, :any)."
  [v t]
  (case t
    :string  (string? v)
    :int     (integer? v)
    :number  (number? v)
    :boolean (instance? Boolean v)
    :any     true
    true))


(defn group-children-by-tag
  "Группирует дочерние элементы doc-node по тегу."
  [doc-node]
  (->> (m/element-children doc-node)
       (filter m/element?)
       (group-by m/element-tag)))


(defn validate
  "Проверяет корневой документ root по схеме schema.
   Возвращает {:ok? true|false, :errors [...]}.
   errors — вектор карт вида:
   {:path [...теги...]
    :kind :тип-ошибки
    :message \"описание\" ...}"
  [schema root]
  (letfn
      [(validate-attrs [schema-node doc-node path]
         (let [attrs-spec (:attrs schema-node {})
               attrs-val  (m/element-attrs doc-node)
               errors     (transient [])]

           (doseq [[k {:keys [required type]}] attrs-spec]
             (when (and required (not (contains? attrs-val k)))
               (conj! errors {:path path
                              :kind :missing-attribute
                              :attribute k
                              :message (format "Отсутствует обязательный атрибут %s" (name k))})))

           (doseq [[k v] attrs-val]
             (if-let [{:keys [type]} (get attrs-spec k)]
               (when (and type (not (value-of-type? v type)))
                 (conj! errors {:path path
                                :kind :wrong-attribute-type
                                :attribute k
                                :expected type
                                :actual v
                                :message (format "Атрибут %s имеет некорректный тип" (name k))}))
               (conj! errors {:path path
                              :kind :unexpected-attribute
                              :attribute k
                              :message (format "Неожиданный атрибут %s" (name k))})))

           (persistent! errors)))

       (validate-text [schema-node doc-node path]
         (let [t        (:text-type schema-node)
               children (m/element-children doc-node)
               texts    (remove m/element? children)
               errors   (transient [])]

           (if (nil? t)
             (when (seq texts)
               (conj! errors {:path path
                              :kind :unexpected-text
                              :message "Текстовое содержимое не допускается этим типом узла"}))
             (doseq [tv texts]
               (when-not (value-of-type? tv t)
                 (conj! errors {:path path
                                :kind :wrong-text-type
                                :expected t
                                :actual tv
                                :message "Текстовое содержимое имеет неверный тип"}))))

           (persistent! errors)))

       (validate-node [schema-node doc-node path]
         (let [schema-tag (:tag schema-node)
               doc-tag    (m/element-tag doc-node)
               errors     (transient [])]

           (when (and schema-tag (not= schema-tag doc-tag))
             (conj! errors {:path path
                            :kind :wrong-tag
                            :expected schema-tag
                            :actual doc-tag
                            :message (format "Ожидался тег %s, получен %s"
                                             (name schema-tag) (name doc-tag))}))

           (let [errors-attrs  (validate-attrs schema-node doc-node path)
                 errors-text   (validate-text schema-node doc-node path)
                 errors-child  (validate-children schema-node doc-node path)]
             (persistent!
              (reduce conj! errors (concat errors-attrs errors-text errors-child))))))

       (validate-children [schema-node doc-node path]
         (let [child-specs (:children schema-node [])
               spec-by-tag (into {} (map (fn [cs] [(:tag cs) cs]) child-specs))
               grouped     (group-children-by-tag doc-node)
               errors      (transient [])]

           (doseq [[tag spec] spec-by-tag]
             (let [children (get grouped tag [])
                   n        (count children)
                   min-o    (or (:min-occurs spec) 0)
                   max-o    (or (:max-occurs spec) 1)]

               (when (< n min-o)
                 (conj! errors {:path path
                                :kind :min-occurs
                                :tag tag
                                :expected min-o
                                :actual n
                                :message (format "Для дочернего узла %s минимальное количество %d, найдено %d"
                                                 (name tag) min-o n)}))

               (when (and (not= max-o :unbounded)
                          (> n max-o))
                 (conj! errors {:path path
                                :kind :max-occurs
                                :tag tag
                                :expected max-o
                                :actual n
                                :message (format "Для дочернего узла %s максимальное количество %d, найдено %d"
                                                 (name tag) max-o n)}))

               (doseq [ch children]
                 (let [subpath (conj path tag)]
                   (doseq [e (validate-node spec ch subpath)]
                     (conj! errors e))))))
           (doseq [[tag _children] grouped
                   :when (not (contains? spec-by-tag tag))]
             (conj! errors {:path path
                            :kind :unexpected-child
                            :tag tag
                            :message (format "Неожиданный дочерний узел %s" (name tag))}))

           (persistent! errors)))]

    (let [errors (validate-node schema root [(m/element-tag root)])]
      {:ok?   (empty? errors)
       :errors errors})))

(def sample-schema
  {:tag :catalog
   :attrs {:lang {:type :string :required true}}
   :text-type nil
   :children [{:tag :book
               :min-occurs 1
               :max-occurs :unbounded
               :attrs {:id   {:type :int :required true}
                       :year {:type :int :required true}}
               :text-type nil
               :children [{:tag :title
                           :min-occurs 1
                           :max-occurs 1
                           :attrs {}
                           :text-type :string
                           :children []}
                          {:tag :author
                           :min-occurs 1
                           :max-occurs 1
                           :attrs {}
                           :text-type :string
                           :children []}]}]})
