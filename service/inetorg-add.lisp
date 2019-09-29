;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

;; ========================================================================== ;;
;; inetorg-add

(defclass inetorg-add (auth-service generic-form)
  ((instructions :initarg :instructions
                 :initform nil
                 :accessor instructions))
  (:documentation ""))

(define-generic-form-constructor (inetorg-add "inetorg-add-form" "/inetorg/add/submit")
    '((:name "add-givenname" :label "givenName" :field-type "text")
      (:name "add-sn" :label "sn" :field-type "text")
      (:name "add-mail" :label "mail" :field-type "text")
      (:name "add-postaladdress" :label "postalAddress" :field-type "text")
      (:name "add-postalcode" :label "postalCode" :field-type "text")
      (:name "add-st" :label "st" :field-type "text")
      (:name "add-l" :label "l" :field-type "text")
      (:name "add-telephonenumber" :label "telephoneNumber" :field-type "text")
      (:name "add-mobile" :label "mobile" :field-type "text")
      (:name "add-businesscategory" :label "businessCategory" :field-type "text")
      (:name "add-submit" :label "Create InetOrg Entry" :field-type "button")))

(defun inetorg-add-json ()
  (with-auth (instance inetorg-add)
    (setf (instructions instance) "Use the form to add a new InetOrg entry.")))

;; ========================================================================== ;;
;; inetorg-add-submit

(defclass inetorg-add-submit (auth-service)
  ((location-p :initarg :location-p
               :initform nil
               :accessor location-p))
  (:documentation ""))

(defun inetorg-add-submit-json (givenname sn mail postaladdress postalcode st l telephonenumber mobile businesscategory)
  (with-auth (instance inetorg-add-submit)
    (with-ldap (ldap)
      (let ((ldap-user (make-instance 'ldap-user
                                      :givenname givenname
                                      :sn sn
                                      :mail mail
                                      :postaladdress postaladdress
                                      :postalcode postalcode
                                      :st st
                                      :l l
                                      :telephonenumber telephonenumber
                                      :mobile mobile
                                      :businesscategory businesscategory)))
        (add-ldap-user ldap-user ldap)
        (setf (message instance) "InetOrg entry created successfully.")))))
