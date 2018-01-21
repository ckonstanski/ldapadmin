;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

;; ========================================================================== ;;

(defgeneric disconnect (ldap)
  (:documentation ""))

(defgeneric get-ldap-users (ldap search-base)
  (:documentation "Calls `with-ldap-iterate' to iterate over all LDAP
users returned with the search `search-base', wrapping each entry in
an `ldap-user' object. Returns a list of these objects. `ldap' is a
free variable that must be present for `with-ldap-iterate'."))

(defgeneric get-ldap-user (ldap cn)
  (:documentation "Calls `with-ldap-users' with a search filter and
returns the first entry."))

