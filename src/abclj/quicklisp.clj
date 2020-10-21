(ns abclj.quicklisp
  (:require [abclj.core :refer [with-cl with-cl->clj cl-load]])
  (:import [java.io File])
  )

(def ^:const ^:private quicklisp-url "https://beta.quicklisp.org/quicklisp.lisp")

(defn install-quicklisp
  "Install quicklisp"
  []
  (let [tmp-file (File/createTempFile "quicklisp" ".lisp")]
    (->> quicklisp-url slurp (spit tmp-file))
    (cl-load tmp-file)
    (when-not (with-cl '(find-package 'ql))
      (with-cl '(quicklisp-quickstart:install)))
    :ok))

(defn load-quicklisp
  "Loads quicklisp setup from the default folder"
  []
  (with-cl `(load ~(str (System/getProperty "user.home")
                   "/quicklisp/setup.lisp"))))

(defn quickload
  "Load libraries from quicklisp, accept n number of CL libraries."
  [& pkgs]
  (load-quicklisp)
  (mapv #(with-cl->clj `(ql:quickload ~%)) pkgs))

(defn system-apropos
  "Search a library in quicklisp"
  [pkg]
  (load-quicklisp)
  (with-cl->clj `(ql:system-apropos ~pkg)))

(defn uninstall
  "Delete a installed library, accept n number of CL libraries"
  [& pkgs]
  (load-quicklisp)
  (mapv #(with-cl->clj `(ql:uninstall ~%)) pkgs))
