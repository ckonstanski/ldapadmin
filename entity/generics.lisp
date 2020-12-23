;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

(defgeneric attribute-value-list (entity &optional keep-nulls)
  (:documentation "Builds an alist of attribute/value pairs."))

(defgeneric get-slots-regex (entity regex)
  (:documentation "Gets an alphabetically sorted list of `entity'
slots whose names match `regex'."))

(defgeneric intersect-slots (entity slots)
  (:documentation "Since the built-in `intersect' function does not
take package name prefixes into account, and since
`org-ckons-core::map-slot-names' returns slot names prefixed with the
package name, this method was written to intersect lists ignoring
package prefixes."))

(defgeneric get-cn (ldap-user)
  (:documentation "Generates the CN of an `ldap-user'. `trivial-ldap'
does not fetch `cn' so we have to assemble it ourselves."))

(defgeneric get-user-dn (ldap-user ldap)
  (:documentation "Generates the full DN of an `ldap-user'."))

(defgeneric modify-ldap-user (ldap-user ldap)
  (:documentation "Writes the data in `ldap-user' to the LDAP
server."))

(defgeneric delete-ldap-user (ldap-user ldap)
  (:documentation "Deletes an `ldap-user' from LDAP."))

(defgeneric add-ldap-user (ldap-user ldap)
  (:documentation "Adds an `ldap-user' to LDAP."))
