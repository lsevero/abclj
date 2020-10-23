(ns abclj.readers
  (:require [clojure.string :as str])
  (:import [org.armedbear.lisp Lisp Interpreter LispInteger Fixnum Ratio DoubleFloat SingleFloat Complex Symbol Nil Packages]
           [java.io Writer]))

(Interpreter/createInstance)

(defn cl-integer
  [form]
  (if (number? form)
    (LispInteger/getInstance (long form))
    (throw (ex-info "Form should be an number!" {:form form}))))

(defmethod print-method LispInteger
  [^Fixnum form ^Writer w]
  (.write w (format "#abclj/cl-integer %d" (.-value form))))

(defmethod print-dup LispInteger
  [^Fixnum form ^Writer w]
  (.write w (format "#abclj/cl-integer %d" (.-value form))))

(defn cl-ratio
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
  [form]
  (prn form)
  (if (and (sequential? form)
           (= 2 (count form))
           (every? number? form))
    (let [[r i] form]
     (Complex/getInstance (cl-double r) (cl-double i)))
    (throw (ex-info "Form should be a sequential of size 2 with numbers only!" {:form form}))))

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
  [form]
  (if (symbol? form)
    (let [ns-form (namespace form)
          name-form (name form)
          ^org.armedbear.lisp.Package pkg (if-not (nil? ns-form)
                                            (-> ns-form str/upper-case Packages/findPackage)
                                            Lisp/PACKAGE_CL_USER)]
        (.intern pkg name-form))
    (throw (ex-info "Form should be a symbol!" {:form form}))))


(defmethod print-method Symbol
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
