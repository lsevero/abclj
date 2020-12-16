(ns abclj.core
  (:refer-clojure :exclude [remove-ns])
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is]]
            [clojure.string :as str]
            [clojure.walk :refer [postwalk]])
  (:import [org.armedbear.lisp EndOfFile Stream StringInputStream LispThread Environment Interpreter Load LispObject
            Lisp LispInteger DoubleFloat SingleFloat AbstractString Symbol Ratio Nil Fixnum Packages SimpleString
            Primitives SpecialBindingsMark Function Closure Cons JavaObject Complex Bignum ComplexString HashTable
            Pathname]
           [abclj.java AbcljUtils]
           [java.io ByteArrayInputStream Writer]))

(Interpreter/createInstance)

(def cl-t (Symbol/T))
(def cl-nil Nil/NIL)

(defprotocol Clojurifiable
  (cl->clj [this]
           "Try to convert a Common Lisp object to a equivalent Clj/Java object"))

(defprotocol CommonLispfiable
  (clj->cl [this]
           "Try to convert a Clj/Java class to a equivalent Common Lisp object"))

(defprotocol SetVar
  (setvar [this obj]
          "Set a value to a symbol.
          The namespace on the symbol will be used as the CL package on the CL symbol.
          If the package is nil then the package COMMON-LISP-USER will be used.
          If object is not a LispObject then clj->cl will be called."))

(defprotocol GetVar
  (getvar [this]
          "Get the symbol value.
          The namespace on the symbol will be used as the CL package on the CL symbol.
          If the package is nil then the package COMMON-LISP-USER will be used. 
          "))

(defprotocol SetFunction
  (setfunction [this obj]
               "Set a function to a symbol.
               The namespace on the symbol will be used as the CL package on the CL symbol.
               If the package is nil then the package COMMON-LISP-USER will be used. 
               "))

(defprotocol GetFunction
  (getfunction [this]
               "Get a function from a symbol
               The namespace on the symbol will be used as the CL package on the CL symbol.
               If the package is nil then the package COMMON-LISP-USER will be used. 
               "))

(defprotocol Evaluatable
  (cl-evaluate [this]
               "Evaluates the form on the Common lisp interpreter"))

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

(defn cl-function?
  "Check if the object is a Common Lisp function"
  [obj]
  (instance? Function obj))

(let [arrayclass (class (make-array LispObject 1))]
  (defn cl-obj-array?
    "Check if obj is instance of LispObject[]"
    [obj]
    (instance? arrayclass obj)))

(defn cl-int
  "Build a Common lisp LispInteger"
  ^LispInteger
  [form]
  (if (number? form)
    (LispInteger/getInstance (long form))
    (throw (ex-info "Form should be an number!" {:form form}))))


(defmethod print-method LispInteger
  [^Fixnum form ^Writer w]
  (.write w (format "#abclj/cl-int %d" (.-value form))))

(defmethod print-dup LispInteger
  [^Fixnum form ^Writer w]
  (.write w (format "#abclj/cl-int %d" (.-value form))))

(defn cl-ratio
  "Builds a Common Lisp Ratio"
  ^Ratio
  [form]
  (if (ratio? form)
    (let [n (numerator form)
          d (denominator form)]
      (Ratio. n d))
    (throw (ex-info "Form should be an ratio!" {:form form}))))

(defmethod print-method Ratio
  [^Ratio form ^Writer w]
  (.write w (format "#abclj/cl-ratio %d/%d" (.numerator form) (.denominator form))))

(defmethod print-dup Ratio
  [^Ratio form ^Writer w]
  (.write w (format "#abclj/cl-ratio %d/%d" (.numerator form) (.denominator form))))

(defn cl-double
  "Builds a Common Lisp DoubleFloat"
  [form]
  (if (number? form)
    (DoubleFloat/getInstance (double form))
    (throw (ex-info "Form should be an number!" {:form form}))))

(defmethod print-method DoubleFloat
  [^DoubleFloat form ^Writer w]
  (.write w (format "#abclj/cl-double %f" (.-value form))))

(defmethod print-dup DoubleFloat
  [^DoubleFloat form ^Writer w]
  (.write w (format "#abclj/cl-double %f" (.-value form))))

(defmethod print-method SingleFloat
  [^SingleFloat form ^Writer w]
  (.write w (format "#abclj/cl-double %f" (.-value form))))

(defmethod print-dup SingleFloat
  [^SingleFloat form ^Writer w]
  (.write w (format "#abclj/cl-double %f" (.-value form))))

(defn cl-complex
  "Builds a common lisp Complex number"
  [form]
  (case (count form)
    2 (let [[r i] form]
        (Complex/getInstance (DoubleFloat/getInstance (double r)) (DoubleFloat/getInstance (double i))))
    4 (let [[r _ i __] form]
        (Complex/getInstance (DoubleFloat/getInstance (double r)) (DoubleFloat/getInstance (double i))))
    (throw (ex-info "Form should be a vector of size 2 with numbers only!" {:form form}))))

(defmethod print-method Complex
  [^Complex form ^Writer w]
  (.write w (format "#abclj/cl-complex [%f %f]"
                    (-> form ^DoubleFloat (.getRealPart) .-value)
                    (-> form ^DoubleFloat (.getImaginaryPart) .-value))))

(defmethod print-dup Complex
  [^Complex form ^Writer w]
  (.write w (format "#abclj/cl-complex [%f %f]"
                    (-> form ^DoubleFloat (.getRealPart) .-value)
                    (-> form ^DoubleFloat (.getImaginaryPart) .-value))))

(defn cl-symbol
  "Builds a common lisp Symbol.
  if a string is provided there should be a '/' or ':' separating the package followed by the symbol name."
  ^Symbol
  [form]
  (cond
    (or (symbol? form)
        (keyword? form)) (let [ns-form (namespace form)
                               name-form (-> form name str/upper-case)
                               ^org.armedbear.lisp.Package pkg (if (keyword? form)
                                                                 Lisp/PACKAGE_KEYWORD
                                                                 (if-not (nil? ns-form)
                                                                   (-> ns-form str/upper-case Packages/findPackage)
                                                                   Lisp/PACKAGE_CL_USER))]
                           (if-let [s (.findAccessibleSymbol pkg name-form)]
                             s
                             (.intern pkg name-form)))
    (string? form) (let [[pkg-or-symbol symbol-or-nil] (str/split form #"/|:")
                         ^org.armedbear.lisp.Package pkg (if (nil? symbol-or-nil)
                                                           Lisp/PACKAGE_CL_USER
                                                           (-> pkg-or-symbol str/upper-case Packages/findPackage))
                         ^String name-form (if (nil? symbol-or-nil)
                                             pkg-or-symbol
                                             symbol-or-nil)]
                     (if-let [s (.findAccessibleSymbol pkg name-form)]
                       s
                       (.intern pkg name-form)))
    :else (throw (ex-info "Form should be a symbol, keyword or string!" {:form form}))))

(defmethod print-method Symbol
  [^Symbol form ^Writer w]
  (.write w (cond
              (= Symbol/T form) "abclj.core/cl-t"
              (= Nil/NIL form) "abclj.core/cl-nil"
              :else (let [^org.armedbear.lisp.Package pkg (-> form .getPackage)
                          sname (-> form .getName)]
                      (format "#abclj/cl-symbol '%s/%s"
                              (if (nil? pkg)
                                "nil"
                                (.getName pkg))
                              sname)))))

(defmethod print-dup Symbol
  [^Symbol form ^Writer w]
  (.write w (cond
              (= Symbol/T form) "abclj.core/cl-t"
              (= Nil/NIL form) "abclj.core/cl-nil"
              :else (let [^Package pkg (-> form .getPackage)
                          sname (-> form .getName)]
                      (format "#abclj/cl-symbol '%s/%s"
                              (if (nil? pkg)
                                "nil"
                                (.getName pkg))
                              sname))))) 

(extend-protocol SetVar
  clojure.lang.Symbol (setvar [sym obj]
                        (let [cl-obj (if (cl-obj? obj)
                                       obj
                                       (clj->cl obj))]
                          (.setSymbolValue (cl-symbol sym) cl-obj)))
  Symbol (setvar [sym obj]
           (let [cl-obj (if (cl-obj? obj)
                          obj
                          (clj->cl obj))]
             (.setSymbolValue sym cl-obj))))

(extend-protocol GetVar
  clojure.lang.Symbol (getvar [sym]
                        (.symbolValue (cl-symbol sym)))
  Symbol (getvar [sym]
           (.symbolValue sym)))

(extend-protocol SetFunction
  clojure.lang.Symbol (setfunction [sym func]
                        {:pre [(is (cl-function? func))]}
                        (.setSymbolFunction (cl-symbol sym) func))
  Symbol (setfunction [sym func]
           {:pre [(is (cl-function? func))]}
           (.setSymbolFunction sym func)))

(extend-protocol GetFunction
  clojure.lang.Symbol (getfunction [sym]
                        (.getSymbolFunctionOrDie (cl-symbol sym)))
  Symbol (getfunction [sym]
           (.getSymbolFunctionOrDie sym)))

(defn funcall
  "Call a CL function, kinda like the CL funcall"
  ([^Function f]
   {:pre [(is (cl-function? f))]}
   (.execute f))
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

(defn princ-to-string
  "Get a string representation of a LispObject as it CL function princ-to-string would return"
  [^LispObject obj]
  {:pre [(is (cl-obj? obj))]}
  (.princToString obj))

(let [cl-prin1 (getfunction 'cl/prin1-to-string)]
  (defn prin1-to-string
    [obj]
    {:pre [(is (cl-obj? obj))]}
    (str (funcall cl-prin1 obj))))


(defn cl-string
  "Builds a Common Lisp SimpleString"
  ^SimpleString
  [^String form]
  (if (string? form)
    (SimpleString. form)
    (throw (ex-info "Form should be a string!" {:form form}))))

(defmethod print-method SimpleString
  [^SimpleString form ^Writer w]
  (.write w (format "#abclj/cl-string %s" (prin1-to-string form))))

(defmethod print-dup SimpleString
  [^SimpleString form ^Writer w]
  (.write w (format "#abclj/cl-string %s" (prin1-to-string form))))

(defmethod print-method ComplexString
  [^SimpleString form ^Writer w]
  (.write w (format "#abclj/cl-string %s" (prin1-to-string form))))

(defmethod print-dup ComplexString
  [^SimpleString form ^Writer w]
  (.write w (format "#abclj/cl-string %s" (prin1-to-string form))))

(defn cl-pathname
  "Builds a Common Lisp Pathname"
  [^String form]
  (if (string? form)
    (Pathname/create form)
    (throw (ex-info "Form should be a string!" {:form form}))))

(let [cl-namestring (getfunction 'cl/namestring)]
  (defmethod print-method Pathname
    [^Pathname form ^Writer w]
    (.write w (format "#abclj/cl-pathname %s" (prin1-to-string (funcall cl-namestring form)))))

  (defmethod print-dup Pathname
    [^Pathname form ^Writer w]
    (.write w (format "#abclj/cl-pathname %s" (prin1-to-string (funcall cl-namestring form))))))

(declare map->alist)

(extend-protocol CommonLispfiable
  Long (clj->cl [obj]
         (cl-int obj))
  Integer (clj->cl [obj]
            (cl-int obj))
  clojure.lang.Ratio (clj->cl [obj]
                       (cl-ratio obj))
  String (clj->cl [obj]
           (cl-string obj))
  clojure.lang.Symbol (clj->cl [obj]
                        (cl-symbol obj))
  clojure.lang.Keyword (clj->cl [obj]
                         (cl-symbol obj))
  Float (clj->cl [obj]
          (cl-double obj))
  Double (clj->cl [obj]
           (cl-double obj))
  Boolean (clj->cl [obj]
            (if obj
              cl-t
              cl-nil))
  nil (clj->cl [_]
        cl-nil)
  clojure.lang.IPersistentMap (clj->cl [obj]
                                (postwalk clj->cl (map->alist obj)))
  Object (clj->cl [obj]
           obj))

(defn cl-cons
  "Builds a Common Lisp Cons object.
  The Cons object is NOT nil terminated automatically, if you want to build a non-dotted cl list you will have to put a cl-nil at the end.
  Example:
  (cl-cons [1 2]) => (1 . 2) 
  (cl-cons [1 2 3 cl-nil) => (1 2 3)
  (cl-cons [[1 2] [3 4]) => ((1 . 2) 3 . 4) 
  (cl-cons [[1 2] [3 4] cl-nil]) => ((1 . 2) (3 . 4)) 
  "
  ^Cons
  [coll]
  (letfn [(rec [obj]
            (if (sequential? obj)
              (cl-cons obj)
              obj))]
    (if (and (sequential? coll)
             (>= (count coll) 2))
      (let [obj-coll (map #(if (cl-obj? %) 
                             %
                             (clj->cl %)) coll)]
        (case (count obj-coll)
          2 (Cons. ^LispObject (rec (first obj-coll)) ^LispObject (rec (second obj-coll)))
          3 (Cons. ^LispObject (rec (first obj-coll)) (Cons. ^LispObject (rec (second obj-coll)) ^LispObject (rec (last obj-coll))))
          (let [firstkons (Cons. ^LispObject (rec (first obj-coll)))
                lastkons (Cons. ^LispObject (rec (-> obj-coll butlast ^LispObject (last))) ^LispObject (rec (last obj-coll)))]
            (loop [[h & t :as newcoll] (-> coll rest butlast butlast)
                   actualkons firstkons]
              (let [newkons (Cons. ^LispObject (clj->cl (rec h)))]
                (set! (.-cdr actualkons) newkons)
                (if t
                  (recur t newkons)
                  (do (set! (.-cdr newkons) lastkons)
                      firstkons)))))))
      (throw (ex-info "Form should be a sequential and of size greater than 1!" {:form coll})))))

(defmethod print-method Cons
  [^Cons form ^Writer w]
  (.write w (format "#abclj/cl-cons %s" (prin1-to-string form))))

(defmethod print-dup Cons
  [^Cons form ^Writer w]
  (.write w (format "#abclj/cl-cons %s" (prin1-to-string form))))

(extend-protocol Evaluatable
  String (cl-evaluate
           [s]
           (when-not (Interpreter/initialized)
             (Interpreter/initializeJLisp))
           (let [stream (StringInputStream. s)
                 thread (LispThread/currentThread)
                 _ (.bindSpecial thread Symbol/DEBUGGER_HOOK AbcljUtils/_DEBUGGER_HOOK_FUNCTION)
                 obj (.read stream false Lisp/EOF false thread Stream/currentReadtable)]
             (if (= obj Lisp/EOF)
               (Lisp/error (EndOfFile. stream))
               (Lisp/eval obj *env* thread))))
  LispObject (cl-evaluate
               [obj]
               (when-not (Interpreter/initialized)
                 (Interpreter/initializeJLisp))
               (let [thread (LispThread/currentThread)
                     _ (.bindSpecial thread Symbol/DEBUGGER_HOOK AbcljUtils/_DEBUGGER_HOOK_FUNCTION)]
                 (Lisp/eval obj *env* thread)))
  clojure.lang.Sequential (cl-evaluate
                            [obj]
                            (when-not (Interpreter/initialized)
                              (Interpreter/initializeJLisp))
                            (let [thread (LispThread/currentThread)
                                  _ (.bindSpecial thread Symbol/DEBUGGER_HOOK AbcljUtils/_DEBUGGER_HOOK_FUNCTION)]
                              (Lisp/eval (cl-cons obj) *env* thread))))

(defn ->bool
  "Get boolean value of Common Lisp object"
  [^LispObject obj]
  {:pre [(is (cl-obj? obj))]}
  (.getBooleanValue obj))

(defn cons->vec
  "Converts a CL cons/list to a clojure vector.
  Only works for nil terminated lists, not dotted ones"
  [^Cons obj]
  {:pre [(is (instance? Cons obj))]}
  (-> obj .copyToArray vec))

(defn dotted-pair?
  "Check if cdr of obj is not nil"
  [^Cons obj]
  {:pre [(is (instance? Cons obj))]}
  (-> obj .-cdr (= cl-nil)))

(let [cl-last (getfunction 'cl/last)]
  (defn dotted-list?
  "Go to the last Cons of Cons and checks if .-cdr is nil"
  [^Cons obj]
  {:pre [(is (instance? Cons obj))]}
  (->> obj (funcall cl-last) dotted-pair?)))

(defn declojurify [coll]
  (letfn [(remove-ns [node]
            (if (symbol? node)
              (-> node name symbol)
              node))]
    (postwalk (fn [node] (-> node
                             remove-ns
                             )) coll)))

(declare alist->map)


(extend-protocol Clojurifiable
  LispInteger (cl->clj [obj]
                (if (= (class obj) Bignum)
                  (.-value ^Bignum obj)
                  (.-value ^Fixnum obj)))
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
  HashTable (cl->clj [obj]
              (postwalk cl->clj (alist->map (.getEntries obj))))
  Object (cl->clj [obj]
           obj))

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

(let [cl-make-hash-table (getfunction 'cl/make-hash-table)]
  (defn map->alist
    "Convests a clojure map to a Common Lisp Eql HashTable"
    [m]
    (let [^HashTable ht (funcall cl-make-hash-table)]
      (doseq [[k v] m]
        (.put ht (clj->cl k) (clj->cl v)))
      ht)))

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

(let [cl-list-all-packages (getfunction 'cl/list-all-packages)]
  (defn list-all-packages
    "Return a Cons with all visible CL packages"
    ^Cons
    []
    (funcall cl-list-all-packages)))

(let [cl-coerce (getfunction 'cl/coerce)]
  (defn coerce
    "Common Lisp coerce function.
    It is hard to work with some CL data structures on the java/clj side.
    Coercing them to list makes things a lot easier. See also cons->vec."
    [^LispObject obj s]
    {:pre [(is (cl-obj? obj)) (is (symbol? s))]}
    (funcall cl-coerce obj (cl-symbol s))))

(defmacro defun
  "Defines a new function named `sym` in the global environment."
  [sym args & body]
  `(let [cl-symb# (cl-evaluate
                   (str (seq (into []
                                   (declojurify
                                    '(defun ~sym ~args ~@body))))))]
     (defn ~sym [~@args]
       (let [cl-objs# (map clj->cl (list ~@args))
             cl-objs# (if (= (count cl-objs#) 0)
                        cl-nil
                        (cl-cons (conj (vec cl-objs#) cl-nil)))
             apply# (.getSymbolFunctionOrDie (cl-symbol (symbol "cl/apply")))]
         (cl->clj
          (funcall apply# (.getSymbolFunctionOrDie cl-symb#) cl-objs#))))))
