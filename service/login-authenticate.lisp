;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

;; ========================================================================== ;;

(defclass login-authenticate (rest-service)
  ((location-p :initarg :location-p
               :initform nil
               :accessor location-p))
  (:documentation ""))

(defmethod initialize-instance :after ((login-authenticate login-authenticate) &key auth-result)
  (if auth-result
      (setf (message login-authenticate) "Successfully logged in.")
      (setf (errormsg login-authenticate) "Login failed.")))

(defun login-authenticate-json (dn password)
  (let ((auth-result nil))
    (when (check-ldap-password (ldap *webapp*) dn password)
      (setf (session-value :permissions) "admin")
      (setf auth-result t))
    (objects-to-json `(,(make-instance 'login-authenticate :auth-result auth-result)))))
