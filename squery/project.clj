(defproject squery "0.1.0-SNAPSHOT"
  :description "Навигация, модификация и валидация данных в виде S-выражений (аналог XPath/XML Schema/XSLT для Clojure)"
  :url "https://example.com/squery"
  :license {:name "EPL-2.0"}
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :plugins [[lein-codox "0.10.8"]]
  :codox {:output-path "target/doc"
          :source-paths ["src"]
          :metadata {:doc "Автоматически сгенерированная документация по API"}}
  :source-paths ["src"]
  :test-paths ["test"]
  :main squery.core)
