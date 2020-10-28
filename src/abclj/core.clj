(ns abclj.core
  (:refer-clojure :exclude [remove-ns])
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is]]
            [clojure.string :as str]
            [clojure.walk :refer [postwalk]]
            [abclj.readers :refer :all])
  (:import [org.armedbear.lisp EndOfFile Stream StringInputStream LispThread Environment Interpreter Load LispObject
            Lisp LispInteger DoubleFloat SingleFloat AbstractString Symbol Ratio Nil Fixnum Packages SimpleString
            Primitives SpecialBindingsMark Function Closure Cons JavaObject]
           [abclj.java AbcljUtils]
           [java.io ByteArrayInputStream]))

(def cl-t (Symbol/T))
(def cl-nil Nil/NIL)

(defn cl-load
  "Execute the contents of the file f in the CL environment.
  Works like the load CL function.
  "
  [f]
  (-> f io/input-stream Load/load))

(defn cl-load-resource
  "Execute the contents of the resource file f in the CL environment.
  Works like the load CL function.
  Useful if you want to embed a CL file in a .jar
  "
  [f]
  (-> f io/resource io/input-stream Load/load))

(defn cl-load-string
  "Execute the contents of the string as it was read from a file in the CL environment"
  [^String s]
  {:pre [(is (string? s))]}
  (-> s .getBytes ByteArrayInputStream. io/input-stream Load/load))

(defn new-env
  "Returns a new environment"
  ^Environment
  []
  (Environment.))

(def ^:dynamic ^Environment *env*
  "ABCLJ environment
  You can overload the *env* with a new one using the binding clojure macro"
  (new-env))

(defn cl-obj?
  "Check if the object is a Common Lisp object"
  [obj]
  (instance? LispObject obj))

(let [arrayclass (class (make-array LispObject 1))]
  (defn cl-obj-array?
    "Check if obj is instance of LispObject[]"
    [obj]
    (instance? arrayclass obj)))

(defn cl-function?
  "Check if the object is a Common Lisp function"
  [obj]
  (instance? Function obj))

(defn princ-to-string
  "Get a string representation of a LispObject as it CL function princ-to-string would return"
  [^LispObject obj]
  {:pre [(is (cl-obj? obj))]}
  (.princToString obj))

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
  {:pre [(is (cl-obj? obj))]}
  (.getBooleanValue obj))

(defn declojurify [coll]
  (letfn [(remove-ns [node]
            (if (symbol? node)
              (-> node name symbol)
              node))]
    (postwalk (fn [node] (-> node
                             remove-ns
                             )) coll)))

(defprotocol Clojurifiable
  (cl->clj [this]))

(extend-protocol Clojurifiable
  LispInteger (cl->clj [obj]
                (.-value ^Fixnum obj))
  SingleFloat (cl->clj [obj]
                (.-value obj))
  DoubleFloat (cl->clj [obj]
                (.-value obj))
  AbstractString (cl->clj [obj]
                   (str obj))
  Nil (cl->clj [_]
        nil)
  Ratio (cl->clj [obj]
          (clojure.lang.Ratio. (.numerator ^Ratio obj) (.denominator ^Ratio obj)))
  Symbol (cl->clj [obj]
           (condp = obj
                cl-t true
                cl-nil nil
                (if (= "KEYWORD" (-> ^Symbol obj ^org.armedbear.lisp.Package (.getPackage) .getName))
                  (-> ^Symbol obj .-name str keyword)
                  (-> ^Symbol obj .-name str symbol))))

  Object (cl->clj [obj]
           obj))

(defprotocol CommonLispfiable
  (clj->cl [this]))

(extend-protocol CommonLispfiable
  Long (clj->cl [obj]
         (cl-integer obj))
  Integer (clj->cl [obj]
            (cl-integer obj))
  clojure.lang.Ratio (clj->cl [obj]
                       (cl-ratio obj))
  String (clj->cl [obj]
           (cl-string obj))
  clojure.lang.Symbol (clj->cl [obj]
                        (cl-symbol obj))
  Float (clj->cl [obj]
          (cl-double obj))
  Double (clj-cl [obj]
           (cl-double))
  Boolean (clj->cl [obj]
            (if obj
              cl-t
              cl-nil)))

(defmacro with-cl
  "Run body as a Common Lisp program, the body should be quoted.
  Bindings will be shared between 'with-cl' calls but does not interfere or access global bindings,
  to set or get global state user the setvar or getvar functions.
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
  Bindings will be shared between 'with-cl->clj' calls but does not interfere or access global bindings,
  to set or get global state user the setvar or getvar functions.
  Return the converted CL object to its clj counterpart.
  Readers macros in the CL source should NOT be used as they will be capture by the clj compiler.
  
  Throws a UnhandledCondition in case of failure.
  "
  ([body]
   `(->> ~body with-cl cl->clj))
  ([headbody & restbody]
   `(do (with-cl->clj ~headbody)
        (with-cl->clj ~@restbody))))


(defn setvar
  "Set a value to a symbol.
  The namespace on the symbol will be used as the CL package on the CL symbol.
  If the package is nil then the package COMMON-LISP-USER will be used."
  [sym obj]
  {:pre [(is (symbol? sym)) (is (cl-obj? obj))]}
   (.setSymbolValue (cl-symbol sym) obj)
    obj)

(defn getvar
  "Get the symbol value.
  The namespace on the symbol will be used as the CL package on the CL symbol.
  If the package is nil then the package COMMON-LISP-USER will be used. 
  "
  [sym]
  {:pre [(is (symbol? sym))]}
  (.symbolValue (cl-symbol sym)))

(defn setfunction
  "Set a function to a symbol.
  The namespace on the symbol will be used as the CL package on the CL symbol.
  If the package is nil then the package COMMON-LISP-USER will be used. 
  "
  [sym func]
  {:pre [(is (symbol? sym) (cl-function? func))]}
  (let []
   (.setSymbolFunction (cl-symbol sym) func)))

(defn getfunction
  "Get a function from a symbol
  The namespace on the symbol will be used as the CL package on the CL symbol.
  If the package is nil then the package COMMON-LISP-USER will be used. 
  "
  [sym]
  {:pre [(is (symbol? sym))]}
  (.getSymbolFunctionOrDie (cl-symbol sym)))

(defn funcall
  "Call a CL function, kinda like the CL funcall"
  ([^Function f arg]
   {:pre [(is (cl-function? f)) (is (or (cl-obj? arg)
                                        (cl-obj-array? arg)))]}
   ;We will need reflection here
   (.execute f arg))
  ([^Function f ^LispObject arg1 ^LispObject arg2]
   {:pre [(is (cl-function? f))]}
   (.execute f arg1 arg2))
  ([^Function f ^LispObject arg1 ^LispObject arg2 ^LispObject arg3]
   {:pre [(is (cl-function? f))]}
   (.execute f arg1 arg2 arg3))
  ([^Function f ^LispObject arg1 ^LispObject arg2 ^LispObject arg3 ^LispObject arg4]
   {:pre [(is (cl-function? f))]}
   (.execute f arg1 arg2 arg3 arg4))
  ([^Function f ^LispObject arg1 ^LispObject arg2 ^LispObject arg3 ^LispObject arg4 ^LispObject arg5]
   {:pre [(is (cl-function? f))]}
   (.execute f arg1 arg2 arg3 arg4 arg5))
  ([^Function f ^LispObject arg1 ^LispObject arg2 ^LispObject arg3 ^LispObject arg4 ^LispObject arg5 ^LispObject arg6]
   {:pre [(is (cl-function? f))]}
   (.execute f arg1 arg2 arg3 arg4 arg5 arg6))
  ([^Function f ^LispObject arg1 ^LispObject arg2 ^LispObject arg3 ^LispObject arg4 ^LispObject arg5 ^LispObject arg6 ^LispObject arg7]
   {:pre [(is (cl-function? f))]}
   (.execute f arg1 arg2 arg3 arg4 arg5 arg6 arg7))
  ([^Function f ^LispObject arg1 ^LispObject arg2 ^LispObject arg3 ^LispObject arg4 ^LispObject arg5 ^LispObject arg6 ^LispObject arg7 ^LispObject arg8]
   {:pre [(is (cl-function? f))]}
   (.execute f arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)))

(defn cons->vec
  "Converts a CL cons/list to a clojure vector.
  Lists will be cl-nil terminated, dotted conses will be not."
  [^Cons obj]
  {:pre [(is (instance? Cons obj))]}
  (-> obj .copyToArray vec))

(defn alist->map
  "Converts an assoc list to a clojure map.
  Be aware that Common Lisp likes to upper-case things, so a cl keyword ':test' will be returned as :TEST"
  [^Cons obj]
  (->> obj
       cons->vec
       (map #(let [car (.-car ^Cons %)
                   cdr (.-cdr ^Cons %)]
               [(cl->clj car) (cl->clj cdr)]))
       (into {})))

(let [cl-coerce (getfunction 'cl/coerce)]
  (defn coerce
    "Common Lisp coerce function.
    It is hard to work with some CL data structures on the java/clj side.
    Coercing them to list makes things a lot easier. See cons->vec."
    [^LispObject obj s]
    {:pre [(is (cl-obj? obj)) (is (symbol? s))]}
    (funcall cl-coerce obj (cl-symbol s))))

