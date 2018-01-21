;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

;; ========================================================================== ;;

(defclass logout (rest-service)
  ((location-p :initarg :location-p
               :initform nil
               :accessor location-p))
  (:documentation ""))

(defmethod initialize-instance :after ((logout logout) &key)
  (setf (message logout) "You are now logged out."))

(defun logout-json ()
  (setf (session-value :permissions) "anonymous")
  (objects-to-json `(,(make-instance 'logout))))
