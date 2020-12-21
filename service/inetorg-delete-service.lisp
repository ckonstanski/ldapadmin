;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

(defclass inetorg-delete (auth-service)
  ((cn :initarg :cn
       :initform nil
       :accessor cn)
   (location-p :initarg :location-p
               :initform nil
               :accessor location-p))
  (:documentation ""))

(defun inetorg-delete-json (cn)
  (with-auth (instance inetorg-delete)
    (setf (cn instance) cn)))

(defclass inetorg-delete-submit (auth-service)
  ()
  (:documentation ""))

(defun inetorg-delete-submit-json (cn)
  (with-auth (instance inetorg-delete-submit)
    (with-ldap (ldap)
      (let ((ldap-user (get-ldap-user ldap cn)))
        (delete-ldap-user ldap-user ldap)
        (setf (message instance) "InetOrg entry deleted successfully.")))))
