;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

(defclass logout-service (rest-service)
  ((location-p :initarg :location-p
               :initform nil
               :accessor location-p))
  (:documentation ""))

(defmethod initialize-instance :after ((logout-service logout-service) &key)
  (setf (message logout-service) "You are now logged out."))

(defun logout-json ()
  (set-user (make-default-user))
  (objects-to-json `(,(make-instance 'logout-service))))
