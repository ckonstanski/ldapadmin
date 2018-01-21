;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

;; ========================================================================== ;;
;; inetorg-modify

(defclass inetorg-modify (auth-service generic-form)
  ((ldap-user-values :initarg :ldap-user-values
                     :initform nil
                     :accessor ldap-user-values)
   (location-p :initarg :location-p
               :initform nil
               :accessor location-p))
  (:documentation ""))

(define-generic-form-constructor (inetorg-modify "inetorg-modify-form" "/inetorg/modify/submit")
    '((:name "modify-givenname" :label "givenName" :field-type "text")
      (:name "modify-sn" :label "sn" :field-type "text")
      (:name "modify-mail" :label "mail" :field-type "text")
      (:name "modify-postaladdress" :label "postalAddress" :field-type "text")
      (:name "modify-postalcode" :label "postalCode" :field-type "text")
      (:name "modify-st" :label "st" :field-type "text")
      (:name "modify-l" :label "l" :field-type "text")
      (:name "modify-telephonenumber" :label "telephoneNumber" :field-type "text")
      (:name "modify-mobile" :label "mobile" :field-type "text")
      (:name "modify-submit" :label "Modify InetOrg Entry" :field-type "button")))

(defun inetorg-modify-json (cn)
  (with-auth (instance inetorg-modify)
    (with-ldap (ldap)
      (setf (ldap-user-values instance) (get-ldap-user ldap cn)))))

;; ========================================================================== ;;
;; inetorg-modify-submit

(defclass inetorg-modify-submit (auth-service)
  ((location-p :initarg :location-p
               :initform nil
               :accessor location-p))
  (:documentation ""))

(defun inetorg-modify-submit-json (givenname sn mail postaladdress postalcode st l telephonenumber mobile)
  (with-auth (instance inetorg-modify-submit)
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
                                      :mobile mobile)))
        (modify-ldap-user ldap-user ldap)
        (setf (message instance) "InetOrg entry saved successfully.")))))
