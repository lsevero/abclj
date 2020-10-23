(ns abclj.core
  (:refer-clojure :exclude [remove-ns])
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is]]
            [clojure.walk :refer [postwalk]]
            [abclj.readers :refer [cl-symbol]])
  (:import [org.armedbear.lisp EndOfFile Stream StringInputStream LispThread Environment Interpreter Load LispObject
            Lisp LispInteger DoubleFloat SingleFloat AbstractString Symbol Ratio Nil Fixnum Packages
            Primitives SpecialBindingsMark]
           [abclj.java AbcljUtils]))

(def cl-t (Symbol/T))
(def cl-nil Nil/NIL)

(defn cl-load
  "Execute the contents of the file f in the CL environment.
  Works like the load CL function.
  
  Throws a UnhandledCondition in case of failure.
  "
  [f]
  (-> f io/input-stream Load/load))

(defn cl-load-resource
  "Execute the contents of the resource file f in the CL environment.
  Works like the load CL function.
  Useful if you want to embed a CL file in a .jar
  
  Throws a UnhandledCondition in case of failure.
  "
  [f]
  (-> f io/resource io/input-stream Load/load))

(cl-load-resource "abclj.lisp")

(def +ABCLJ-PKG+
  "ABCLJ Common Lisp Package, defined abclj variables will go to the *objects* variable."
  (Packages/findPackage "ABCLJ"))

(defn new-env
  "Creates a new CL environment"
  []
  (Environment.))

(def ^:dynamic ^Environment *env*
  "ABCLJ environment, all bidings definitions done in a with-cl macro will be hold here.
  You can overload the *env* with a new one using the binding clojure macro"
  (new-env))

(defn copy-env
  "Copy a CL environment"
  [env]
  {:pre [(is (instance? Environment env))]}
  (Environment. env))

(defn cl-obj?
  "Check if the object is a Common Lisp object"
  [obj]
  (instance? LispObject obj))

(defn cl-evaluate
  "Evaluate a CL string source, beahave most like the Interpreter/evaluate but you have control on the environment used."
  [s]
  {:pre [(is (string? s))]}
  (when-not (Interpreter/initialized)
    (Interpreter/initializeJLisp))
  (let [stream (StringInputStream. s)
        thread (LispThread/currentThread)
        _ (.bindSpecial thread Symbol/DEBUGGER_HOOK AbcljUtils/_DEBUGGER_HOOK_FUNCTION)
        obj (.read stream false Lisp/EOF false thread Stream/currentReadtable)]
      (if (= obj Lisp/EOF)
        (Lisp/error (EndOfFile. stream))
        (Lisp/eval obj *env* thread))))

(defn ->bool
  "Get boolean value of Common Lisp object"
  [^LispObject obj]
  {:pre [(is (instance? LispObject obj))]}
  (.getBooleanValue obj))

(defn declojurify [coll]
  (letfn [(remove-ns [node]
            (if (symbol? node)
              (-> node name symbol)
              node))]
    (postwalk (fn [node] (-> node
                             remove-ns
                             )) coll)))

(defn cl->clj
  "Convert CL objects to its clojure counterparts, if no classes match will return the argument unmodified."
  [obj]
  (condp instance? obj
       LispInteger (.-value ^Fixnum obj)
       SingleFloat (.-value ^SingleFloat obj)
       DoubleFloat (.-value ^DoubleFloat obj)
       AbstractString (str obj)       
       Nil nil
       Ratio (clojure.lang.Ratio. (.numerator ^Ratio obj) (.denominator ^Ratio obj))
       Symbol (condp = obj
                cl-t true
                cl-nil nil
                (if (= "KEYWORD" (-> ^Symbol obj ^org.armedbear.lisp.Package (.getPackage) .getName))
                  (-> ^Symbol obj .-name str keyword)
                  (-> ^Symbol obj .-name str symbol)))
       obj))

(defmacro with-cl
  "Run body as a Common Lisp program, the body should be quoted.
  Return the CL object of last form. 
  Readers macros in the CL source should NOT be used as they will be capture by the clj compiler.
  
  Throws a UnhandledCondition in case of failure.
  "
  ([body]
   `(->> ~body declojurify (into []) seq str cl-evaluate))
  ([headbody & restbody]
   `(do (with-cl ~headbody)
        (with-cl ~@restbody))))

(defmacro with-cl->clj
  "Run body as a Common Lisp program, the body should be quoted.
  Return the converted CL object to its cljl counterpart.
  Readers macros in the CL source should NOT be used as they will be capture by the clj compiler.
  
  Throws a UnhandledCondition in case of failure.
  "
  ([body]
   `(->> ~body declojurify (into []) seq str cl-evaluate cl->clj))
  ([headbody & restbody]
   `(do (with-cl->clj ~headbody)
        (with-cl->clj ~@restbody))))


(defn defvar
  "Add a new biding on the current environment, kinda like defvar in CL.
  The namespace on the symbol will be used as the CL package on the CL symbol.
  If the package is nil then the package COMMON-LISP-USER will be used."
  [sym obj]
  {:pre [(is (symbol? sym)) (is (cl-obj? obj))]}
  (let [name-sym (-> sym name symbol)
        cl-sym (cl-symbol sym)]
    (with-cl `(defvar ~name-sym nil))
    (.setSymbolValue cl-sym obj)
    (.bind *env* cl-sym obj)
    obj))

(comment 
         
         (defvar 'hue #abclj/cl-integer 123)
         (.lookup *env* (cl-symbol 'hue))
         (Packages/findPackage "ABCLJ")
         (.findSymbol Lisp/PACKAGE_CL_USER "hue")
         (.findSymbol Lisp/PACKAGE_CL_USER "x")
         (with-cl->clj '(progn (format nil "~a" hue)))
         (with-cl->clj '(print hue))
         (.getName (.getPackage (with-cl '(defvar x 1))))
         (with-cl->clj '(defvar hue nil))
         )

(comment (let []))
