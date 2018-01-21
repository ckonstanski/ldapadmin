;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

;; ========================================================================== ;;

(defclass login (generic-form)
  ()
  (:documentation ""))

(define-generic-form-constructor (login "login-form" "/login/authenticate")
    '((:name "dn" :label "DN" :field-type "text")
      (:name "password" :label "Password" :field-type "password")
      (:name "submit" :label "Login" :field-type "button")))

(defun login-json ()
  (objects-to-json `(,(make-instance 'login))))
