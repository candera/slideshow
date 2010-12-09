(defproject slideshow "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]]
  :dev-dependencies [[swank-clojure "1.2.1"]]
  :main wangdera.slideshow.core
  ;; This next option has to do with Clojure bug #322. Something
  ;; about transitive AOT results in not enough going into the
  ;; uberjar, which means it won't run.
  :keep-non-project-classes true)
