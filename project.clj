(defproject wish-engine "0.1.0-SNAPSHOT"
  :url "https://github.com/dhleong/wish-engine"

  :dependencies
  ;; always use "provided" for Clojure(Script)
  [[org.clojure/clojure "1.10.1" :scope "provided"]
   [org.clojure/clojurescript "1.10.742" :scope "provided"]
   [com.rpl/specter "1.1.3"]]

  :codox {:language :clojurescript}
  :plugins [[lein-codox "0.10.7"]]

  :source-paths
  ["src/engine/main"])
