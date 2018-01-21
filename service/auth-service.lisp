;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

;; ========================================================================== ;;

(defclass auth-service (rest-service)
  ()
  (:documentation ""))

(defmethod initialize-instance :after ((auth-service auth-service) &key)
  (when (not (string= (session-value :permissions) "admin"))
    (setf (location auth-service) "home")
    (setf (errormsg auth-service) "You are not authorized to access this resource.")))

(defmacro with-auth ((instance form-class) &body body)
  `(let ((,instance (make-instance ',form-class)))
     (when (null (errormsg ,instance))
       ,@body)
     (objects-to-json `(,,instance))))
