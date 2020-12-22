;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

(defclass inetorg-modify-service (auth-service)
  ((form :initarg :form
         :initform nil
         :accessor form)
   (ldap-user-values :initarg :ldap-user-values
                     :initform nil
                     :accessor ldap-user-values)
   (title :initarg :title
          :initform nil
          :accessor title)
   (location-p :initarg :location-p
               :initform nil
               :accessor location-p))
  (:documentation ""))

(defmethod initialize-instance :after ((inetorg-modify-service inetorg-modify-service) &key)
  (setf (form inetorg-modify-service) (make-form "inetorg-modify-form"
                                                 nil
                                                 t
                                                 `((:name "modify-givenname" :label "givenName" :field-type "text")
                                                   (:name "modify-sn" :label "sn" :field-type "text")
                                                   (:name "modify-mail" :label "mail" :field-type "text")
                                                   (:name "modify-postaladdress" :label "postalAddress" :field-type "text")
                                                   (:name "modify-postalcode" :label "postalCode" :field-type "text")
                                                   (:name "modify-st" :label "st" :field-type "text")
                                                   (:name "modify-l" :label "l" :field-type "text")
                                                   (:name "modify-telephonenumber" :label "telephoneNumber" :field-type "text")
                                                   (:name "modify-mobile" :label "mobile" :field-type "text")
                                                   (:name "modify-businesscategory" :label "businessCategory" :field-type "text")
                                                   (:label "Modify InetOrg Entry" :field-type "button" :onclick "on_inetorg_modify_submit_clicked()")))))

(defun inetorg-modify-json (cn)
  (with-auth (instance inetorg-modify-service)
    (with-ldap (ldap)
      (setf (ldap-user-values instance) (get-ldap-user ldap cn)))))

(defclass inetorg-modify-submit-service (auth-service)
  ((location-p :initarg :location-p
               :initform nil
               :accessor location-p))
  (:documentation ""))

(defun inetorg-modify-submit-json (givenname sn mail postaladdress postalcode st l telephonenumber mobile businesscategory)
  (with-auth (instance inetorg-modify-submit-service)
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
        (modify-ldap-user ldap-user ldap)
        (setf (message instance) "InetOrg entry saved successfully.")))))
