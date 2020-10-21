(ns abclj.core
  (:refer-clojure :exclude [remove-ns])
  (:require [clojure.java.io :as io]
            [clojure.walk :refer [postwalk]]
            [abclj.readers])
  (:import [org.armedbear.lisp EndOfFile Stream StringInputStream LispThread Environment Interpreter Load LispObject
            Lisp LispInteger DoubleFloat SingleFloat AbstractString Symbol Ratio Nil Fixnum
            Primitives SpecialBindingsMark]))

(def cl-t (Symbol/T))
(def cl-nil Nil/NIL)

(def ^:dynamic *env*
  "ABCLJ environment, all bidings definitions done in a with-cl macro will be hold here.
  You can overload the *env* with a new one using the binding clojure macro"
  (Environment.))

(defn cl-evaluate
  "Evaluate a CL string source, beahave most like the Interpreter/evaluate but you have control on the environment used."
  [s]
  {:pre [(string? s)]}
  (when-not (Interpreter/initialized)
    (Interpreter/initializeJLisp))
  (let [stream (StringInputStream. s)
        thread (LispThread/currentThread)
        obj (.read stream false Lisp/EOF false thread Stream/currentReadtable)]
      (if (= obj Lisp/EOF)
        (Lisp/error (EndOfFile. stream))
        (Lisp/eval obj *env* thread))))

(defn ->bool
  "Get boolean value of Common Lisp object"
  [^LispObject obj]
  (.getBooleanValue obj))

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
(comment (with-cl->clj '(progn '(("a" . 1) ("b" . 3)))))

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

(comment (with-cl
           '(progn 
              (defun fact (n)
                (reduce #'* (loop for i from 1 to n collect i)))
              (fact 5))))

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

(comment (with-cl->clj
           '(progn 
              (defun fact (n)
                (reduce (function *) (loop for i from 1 to n collect i)))
              (fact 5))))
(comment (with-cl->clj
           '(defun fact (n)
              (reduce (function *) (loop for i from 1 to n collect i)))
           '(fact 5)))
(comment (instance? Nil (with-cl '(progn nil))))
(comment (->bool (with-cl '(progn nil))))
(comment (-> '(progn 1) with-cl ->bool))

(comment (with-cl
           '(progn

              (defclass ellipse ()
                ((h-axis :type real :accessor h-axis :initarg :h-axis)
                 (v-axis :type real :accessor v-axis :initarg :v-axis)))

              (defclass circle (ellipse)
                ((radius :type real :accessor radius :initarg :radius)))

              (defmethod initialize-instance ((c circle) &key radius)
                (setf (radius c) radius)) ;; via the setf method below

              (defmethod (setf radius) :after ((new-value real) (c circle))
                (setf (slot-value c 'h-axis) new-value
                      (slot-value c 'v-axis) new-value))

              (defmethod (setf h-axis) :after ((new-value real) (c circle))
                (unless (= (radius c) new-value)
                        (change-class c 'ellipse)))

              (defmethod (setf v-axis) :after ((new-value real) (c circle))
                (unless (= (radius c) new-value)
                        (change-class c 'ellipse)))

              (defmethod initialize-instance :after ((e ellipse) &key h-axis v-axis)
                (if (= h-axis v-axis)
                  (change-class e 'circle)))

              (defmethod (setf h-axis) :after ((new-value real) (e ellipse))
                (unless (subtypep (class-of e) 'circle)
                        (if (= (h-axis e) (v-axis e))
                          (change-class e 'circle))))

              (defmethod (setf v-axis) :after ((new-value real) (e ellipse))
                (unless (subtypep (class-of e) 'circle)
                        (if (= (h-axis e) (v-axis e))
                          (change-class e 'circle))))

              (defmethod update-instance-for-different-class :after ((old-e ellipse)
                                                                     (new-c circle) &key)
                (setf (radius new-c) (h-axis old-e))
                (unless (= (h-axis old-e) (v-axis old-e))
                        (error "ellipse ~s can't change into a circle because it's not one!"
                               old-e)))

              )))
(comment (with-cl '(ql:quickload "vecto")))
(comment (with-cl '(defvar v 1))
         (with-cl '(format nil "v = ~a" v) )
         )
