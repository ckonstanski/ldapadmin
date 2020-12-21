;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

(defclass login-service (rest-service)
  ((form :initarg :form
         :initform nil
         :accessor form)
   (title :initarg :title
          :initform nil
          :accessor title))
  (:documentation ""))

(defmethod initialize-instance :after ((login-service login-service) &key)
  (setf (title login-service) "Login")
  (setf (form login-service) (make-form "login-form"
                                        nil
                                        t
                                        '((:name "dn" :label "DN" :field-type "text" :required "required")
                                          (:name "password" :label "Password" :field-type "password" :required "required")
                                          (:label "Login" :field-type "button" :onclick "on_login_submit_clicked()")))))

(defun login-json ()
  (objects-to-json `(,(make-instance 'login-service))))

(defclass login-authenticate-service (rest-service)
  ((location-p :initarg :location-p
               :initform nil
               :accessor location-p))
  (:documentation ""))

(defmethod initialize-instance :after ((login-authenticate-service login-authenticate-service) &key auth-result)
  (if auth-result
      (setf (message login-authenticate-service) "Successfully logged in.")
      (setf (errormsg login-authenticate-service) "Login failed.")))

(defun login-authenticate-json (username pwd)
  (let ((auth-result nil))
    (when (check-ldap-password (ldap *webapp*) dn password)
      (setf (session-value :permissions) "admin")
      (setf auth-result t))
    (objects-to-json `(,(make-instance 'login-authenticate-service :auth-result auth-result)))))
