;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

(defclass inetorg-add-service (auth-service)
  ((form :initarg :form
         :initform nil
         :accessor form)
   (title :initarg :title
          :initform nil
          :accessor title))
  (:documentation ""))

(defmethod initialize-instance :after ((inetorg-add-service inetorg-add-service) &key)
  (setf (title inetorg-add-service) "Add InetOrg Entry")
  (setf (form inetorg-add-service) (make-form "inetorg-add-form"
                                              nil
                                              t
                                              '((:name "add-givenname" :label "givenName" :field-type "text" :required "required")
                                                (:name "add-sn" :label "sn" :field-type "text" :required "required")
                                                (:name "add-mail" :label "mail" :field-type "text")
                                                (:name "add-postaladdress" :label "postalAddress" :field-type "text")
                                                (:name "add-postalcode" :label "postalCode" :field-type "text")
                                                (:name "add-st" :label "st" :field-type "text")
                                                (:name "add-l" :label "l" :field-type "text")
                                                (:name "add-telephonenumber" :label "telephoneNumber" :field-type "text")
                                                (:name "add-mobile" :label "mobile" :field-type "text")
                                                (:name "add-businesscategory" :label "businessCategory" :field-type "text")
                                                (:label "Create InetOrg Entry" :field-type "button" :onclick "on_inetorg_add_submit_clicked()")))))

(defun inetorg-add-json ()
  (with-auth (instance inetorg-add-service)
    t))

(defclass inetorg-add-submit-service (auth-service)
  ((location-p :initarg :location-p
               :initform nil
               :accessor location-p))
  (:documentation ""))

(defun inetorg-add-submit-json (givenname sn mail postaladdress postalcode st l telephonenumber mobile businesscategory)
  (with-auth (instance inetorg-add-submit-service)
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
