(defproject abclj "0.1.5"
  :description "Armed Bear Clojure, dead easy Common Lisp interop"
  :url "https://www.github.com/lsevero/abclj"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.abcl/abcl "1.8.0"]
                 [org.abcl/abcl-contrib "1.8.0"]]
  :profiles {:dev {:plugins [[cider/cider-nrepl "0.24.0"]]
                   :repl-options {:init-ns abclj.core}}}
  :source-paths ["src"]
  :java-source-paths ["java"])
