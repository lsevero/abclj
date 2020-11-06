(ns webserver.hunchentoot
  (:require [abclj.quicklisp :refer [quickload]]
            [abclj.core :refer [defun with-cl]]))

(quickload "hunchentoot")
(quickload "cl-json")
(quickload "alexandria")

(defun server ()
  (defvar *acceptor*)
  (setq *acceptor* (make-instance 'hunchentoot:easy-acceptor :port 4242))
  (hunchentoot:start *acceptor*))

(server)

(defun add-handler:yo ()
  (hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
                                   (setf (hunchentoot:content-type*) "text/plain")
                                   (let* ((alist-data (json:decode-json-from-string (hunchentoot:raw-post-data :force-text t)))
                                          (data (alexandria:alist-hash-table alist-data)))
                                     (format nil "My name is ~a, but username: ~a and password ~a~%"
                                             name
                                             (gethash :USERNAME data)
                                             (gethash :PASSWORD data)))))

(add-handler:yo)

(defun add-handler:capitalize ()
  (hunchentoot:define-easy-handler (say-cap :uri "/cap") (name)
                                   (setf (hunchentoot:content-type*) "application/json")
                                   (let* ((alist-data (json:decode-json-from-string (hunchentoot:raw-post-data :force-text t)))
                                          (data (alexandria:alist-hash-table alist-data))
                                          (*ht* (make-hash-table)))
                                     (loop for k being the hash-keys in data using (hash-value v)
                                           do (if (stringp v)
                                                (setf (gethash k *ht*) (string-upcase v))
                                                (setf (gethash k *ht*) v)))
                                     (format nil "~a~%" (json:encode-json-to-string *ht*)))))

(add-handler:capitalize)


;;; curl used in test
;;; curl --header "Content-Type: application/json" --request POST --data '{"username":"bartuka","password":12345}' http://localhost:4243/yo?name=Wand

(comment
  (with-cl
   '(hunchentoot:stop *acceptor*)))
