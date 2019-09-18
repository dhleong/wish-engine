(defproject wish-engine "0.1.0-SNAPSHOT"
  :url "https://github.com/dhleong/wish-engine"

  :dependencies
  ;; always use "provided" for Clojure(Script)
  [[org.clojure/clojurescript "1.10.520" :scope "provided"]
   [com.rpl/specter "1.1.2"]]

  :source-paths
  ["src/engine/main"])
