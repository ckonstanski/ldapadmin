;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

(defclass ldap-user (entity)
  ((givenname :initarg :givenname
              :initform nil
              :accessor givenname)
   (sn :initarg :sn
       :initform nil
       :accessor sn)
   (mail :initarg :mail
         :initform nil
         :accessor mail)
   (postaladdress :initarg :postaladdress
                  :initform nil
                  :accessor postaladdress)
   (postalcode :initarg :postalcode
               :initform nil
               :accessor postalcode)
   (st :initarg :st
       :initform nil
       :accessor st)
   (l :initarg :l
      :initform nil
      :accessor l)
   (telephonenumber :initarg :telephonenumber
                    :initform nil
                    :accessor telephonenumber)
   (mobile :initarg :mobile
           :initform nil
           :accessor mobile)
   (businesscategory :initarg :businesscategory
                     :initform nil
                     :accessor businesscategory))
  (:documentation "A single inetOrgPerson entry from LDAP."))

(defmethod get-cn ((ldap-user ldap-user))
  (format nil "~a ~a" (givenname ldap-user) (sn ldap-user)))

(defmethod get-user-dn ((ldap-user ldap-user) (ldap ldap))
  (format nil "cn=~a ~a,ou=people,~a" (givenname ldap-user) (sn ldap-user) (base-dn ldap)))

(defmethod modify-ldap-user ((ldap-user ldap-user) (ldap ldap))
  (let* ((existing-user (get-ldap-user ldap (get-cn ldap-user)))
         (existing-attrs (attribute-value-list existing-user t))
         (new-attrs (attribute-value-list ldap-user))
         (ldap-entry (ldap:new-entry (get-user-dn ldap-user ldap) :attrs existing-attrs))
         (change-attrs (remove-if #'null
                                  (mapcar (lambda (attr)
                                            (let ((new-attr (assoc (car attr) new-attrs)))
                                              (cond ((and (null-or-empty-p (cdr attr))
                                                          (not (null-or-empty-p (cdr new-attr))))
                                                     `(ldap:add ,(car attr) ,(cdr new-attr)))
                                                    ((and (not (null-or-empty-p (cdr attr)))
                                                          (null-or-empty-p (cdr new-attr)))
                                                     `(ldap:delete ,(car attr) ,(cdr attr)))
                                                    ((and (not (null-or-empty-p (cdr attr)))
                                                          (not (null-or-empty-p (cdr new-attr)))
                                                          (not (string= (cdr attr) (cdr new-attr))))
                                                     `(ldap:replace ,(car attr) ,(cdr new-attr))))))
                                          existing-attrs))))
    (ldap:modify (connection ldap) (get-user-dn ldap-user ldap) change-attrs)))

(defmethod delete-ldap-user ((ldap-user ldap-user) (ldap ldap))
  (let* ((existing-user (get-ldap-user ldap (get-cn ldap-user)))
         (existing-attrs (attribute-value-list existing-user))
         (ldap-entry (ldap:new-entry (get-user-dn ldap-user ldap) :attrs existing-attrs)))
    (ldap:delete ldap-entry (connection ldap))))

(defmethod add-ldap-user ((ldap-user ldap-user) (ldap ldap))
  (let* ((new-attrs (attribute-value-list ldap-user))
         (new-entry (ldap:new-entry (get-user-dn ldap-user ldap) :attrs (add-to-list new-attrs '((objectclass . (inetorgperson)))))))
    (ldap:add new-entry (connection ldap))))
