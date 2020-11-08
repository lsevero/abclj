(ns abclj.core-test
  (:import [org.armedbear.lisp Lisp LispObject LispInteger Cons StandardObject]
           [abclj.java UnhandledCondition])
  (:require [clojure.test :refer :all]
            [abclj
             [core :refer :all]]))

(deftest with-cl-test
  (testing
    (is (instance? LispInteger (with-cl '(progn 1))))
    (is (instance? Cons (with-cl '(progn '(1 . 2)
                                         '(3 . 4)))))
    (is (instance? StandardObject (with-cl
                                    '(progn
                                       (defclass ellipse ()
                                         ((h-axis :type real :accessor h-axis :initarg :h-axis)
                                          (v-axis :type real :accessor v-axis :initarg :v-axis)))
                                       (defclass circle (ellipse)
                                         ((radius :type real :accessor radius :initarg :radius)))
                                       (defmethod initialize-instance ((c circle) &key radius)
                                         (setf (radius c) radius))
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
                                                        old-e)))))))))

(deftest ->bool-test
  (testing "Everything that is not cl-nil should be true"
    (is (->bool #abclj/cl-int 1))
    (is (->bool #abclj/cl-double 2.0))
    (is (->bool #abclj/cl-ratio 2/3))
    (is (->bool #abclj/cl-complex [1 1]))
    (is (->bool #abclj/cl-string "abc"))
    (is (->bool (cl-symbol :test)))
    (is (->bool (cl-symbol 'test)))
    (is (false? (->bool cl-nil)))))

(deftest with-cl->clj-test
  (testing
    (is (= 120 (with-cl->clj
                 '(defun fact (n)
                    (reduce (function *) (loop for i from 1 to n collect i)))
                 '(fact 5))))))

(deftest set-get-var-test
  (testing
    (is (= 123 (cl->clj (do (setvar 'test #abclj/cl-int 123)
                            (getvar 'test)))))))

(deftest set-get-function-test
  (testing
    (let [+-cl-func (-> Lisp/PACKAGE_CL
                        (.findAccessibleSymbol "+")
                        .getSymbolFunction)]
      (is (= 3 (cl->clj (.execute +-cl-func #abclj/cl-int 1 #abclj/cl-int 2)))))
    (is (= -1.0 (cl->clj (.-realpart (funcall (getfunction 'cl/expt)
                                              #abclj/cl-complex [0 1]
                                              #abclj/cl-int 2)))))))

(deftest alist->map-test
  (testing
    (is (= {:A "hue"
            :B "br"}
           (alist->map (with-cl '(progn '((:a . "hue")
                                          (:b . "br")))))))))

(deftest coerce-test
  (testing
    (is (instance? Cons (coerce (with-cl '(make-array '(3))) 'list)))))

(deftest cl-cons-test
  (testing
    (is (= "(1 2 3)" (prin1-to-string (cl-cons [1 2 3 cl-nil]))))
    (is (= "(1 2 . 3)" (prin1-to-string (cl-cons [1 2 3]))))
    (is (= "((1 . 2) (3 . 4) (5 . 6))" (prin1-to-string (cl-cons [[1 2]
                                                                  [3 4]
                                                                  [5 6]
                                                                  cl-nil]))))
    (is (= "((1 . 2) (3 . 4) 5 . 6)" (prin1-to-string (cl-cons [[1 2]
                                                                [3 4]
                                                                [5 6]]))))))

(deftest cl-evaluate-test
  (testing
    (is (= 3 (cl->clj (cl-evaluate "(+ 1 2)"))))
    (is (= 3 (cl->clj (cl-evaluate (cl-cons [(cl-symbol 'cl/+) 1 2 cl-nil])))))
    (is (= 3 (cl->clj (cl-evaluate ['cl/+ 1 2 nil]))))
    (is (= 2 (cl->clj (cl-evaluate [(cl-symbol "cl/1+") 1  nil]))))
    (is (= 0 (cl->clj (cl-evaluate [(cl-symbol "cl:1-") 1  nil]))))
    (is (= :EQUAL (cl->clj (cl-evaluate (cl-cons ['cl/if ['cl/= 1 1.0 nil] :equal :not-equal nil])))))
    (is (= :EQUAL (cl->clj (cl-evaluate (cl-cons (list 'cl/if (list 'cl/= 1 1.0 nil) :equal :not-equal nil))))))))

(deftest cl->clj-test
  (testing
    (let [ht (with-cl
               '(defparameter *m* (make-hash-table))
               '(setf (gethash :a *m*) 1)
               '(setf (gethash :b *m*) 2)
               '(setf (gethash :c *m*) 3)
               '(progn *m*))]
      (is (= {:A 1 :B 2 :C 3} (cl->clj ht))))))

(deftest clj->cl-test
  (testing 
    (let [ht (clj->cl {:a 1 :b 2 :c 3})]
      (is (= #abclj/cl-int 1 (.get ht (cl-symbol 'keyword/a))))
      (is (= #abclj/cl-int 2 (.get ht (cl-symbol 'keyword/b))))
      (is (= #abclj/cl-int 3 (.get ht (cl-symbol 'keyword/c))))
      (is (= nil (.get ht (cl-symbol 'keyword/d)))))))

(defun adding (a b)
  (+ a b))

(defun multiply (a b)
  (* a b))

(defun blah (a b)
  (let ((add (adding a b))
        (mul (multiply a b)))
    (+ mul add)))

(deftest defun-with-nested-fns
  (testing
    (is (= 3 (adding 1 2)))
    (is (= 2 (multiply 1 2)))
    (is (= 5 (blah 1 2)))))
