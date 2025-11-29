(ns squery.path
  "Описание и нормализация путей навигации по S-выражениям."
  (:require [squery.model :as m]))


(def any-depth
  "Специальный сегмент переменной вложенности (аналог // в XPath)."
  {:type :any-depth})

(defn any-depth-seg
  "Удобный конструктор сегмента переменной вложенности."
  []
  any-depth)

(defn any-depth-segment?
  "Проверяет, является ли seg сегментом переменной вложенности."
  [seg]
  (= (:type seg) :any-depth))


(defn normalize-segment
  "Нормализует задание сегмента пути во внутренний вид:
   * keyword => {:type :segment, :tag kw, :attrs {}, :pred nil}
   * [:tag {:id 1}] или [:tag {:attrs {:id 1}}]
   * {:tag :book :attrs {:id 1}}
   * :** или {:type :any-depth} — сегмент переменной вложенности."
  [seg]
  (cond
    (and (map? seg) (= (:type seg) :any-depth))
    seg

    (= seg :**)
    {:type :any-depth}

    (keyword? seg)
    {:type :segment
     :tag seg
     :attrs {}
     :pred nil}

    (vector? seg)
    (let [[tag maybe-attrs] seg]
      (when-not (keyword? tag)
        (throw (ex-info "Первый элемент в векторе-сегменте должен быть keyword"
                        {:segment seg})))
      (let [attrs (cond
                    (nil? maybe-attrs) {}
                    (map? maybe-attrs)
                    (if (contains? maybe-attrs :attrs)
                      (:attrs maybe-attrs)
                      maybe-attrs)
                    :else {})]
        {:type :segment
         :tag tag
         :attrs attrs
         :pred nil}))

    (map? seg)
    (let [tag   (:tag seg)
          attrs (:attrs seg)
          pred  (:pred seg)]
      {:type :segment
       :tag tag
       :attrs (or attrs {})
       :pred pred})

    :else
    (throw (ex-info "Неизвестный формат сегмента пути"
                    {:segment seg}))))

(defn segment-matches?
  "Проверяет, удовлетворяет ли узел node сегменту seg (нормализованному)."
  [seg node]
  (when (= (:type seg) :segment)
    (let [tag      (m/element-tag node)
          attrs    (m/element-attrs node)
          seg-tag  (:tag seg)
          seg-attrs (:attrs seg)
          pred     (:pred seg)]
      (and
       (or (nil? seg-tag) (= seg-tag tag))
       (every? (fn [[k v]]
                 (= (get attrs k) v))
               seg-attrs)
       (if pred
         (boolean (pred node))
         true)))))


(defn path-map?
  [p]
  (and (map? p)
       (contains? p :kind)
       (contains? p :segments)))

(defn normalize-path
  "Нормализует путь:
   * если p – map {:kind :absolute|:relative, :segments [...]}
   * если p – просто последовательность сегментов (относительный путь)."
  [p]
  (cond
    (path-map? p)
    (update p :segments (fn [segs]
                          (->> segs (map normalize-segment) vec)))

    (sequential? p)
    {:kind :relative
     :segments (->> p (map normalize-segment) vec)}

    :else
    (throw (ex-info "Неизвестный формат пути" {:path p}))))

(defn absolute
  "Создаёт абсолютный путь из сегментов."
  [& segments]
  (normalize-path {:kind :absolute
                   :segments (vec segments)}))

(defn relative
  "Создаёт относительный путь из сегментов."
  [& segments]
  (normalize-path {:kind :relative
                   :segments (vec segments)}))
