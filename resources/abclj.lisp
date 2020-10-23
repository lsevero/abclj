(in-package :cl)

(defpackage :abclj
  (:use :cl)
  (:export *objects* put get))

(in-package :abclj)

(defvar *objects* (make-hash-table)
  "Container for dynamically generated objects we want to expose to the
  package's user.")

(defun put (name obj)
  (setf (gethash name *objects*) obj))

(defun get (name &optional default)
  (gethash name *objects* default))
