;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

(defclass inetorg-view (auth-service)
  ((title :initarg :title
          :initform nil
          :accessor title))
  (:documentation ""))

(defun inetorg-view-json ()
  (with-auth (instance inetorg-view)
    (setf (title instance) "Use the form to filter the InetOrg results.")))

(defclass inetorg-view-search-service (auth-service)
  ((form :initarg :form
         :initform nil
         :accessor form)
   (title :initarg :title
          :initform nil
          :accessor title)
   (location-p :initarg :location-p
               :initform nil
               :accessor location-p))
  (:documentation ""))

(defmethod initialize-instance :after ((inetorg-view-search-service inetorg-view-search-service) &key)
  (setf (form inetorg-view-search-service) (make-form "inetorg-view-search-form"
                                                      nil
                                                      t
                                                      '((:name "view-givenname" :label "givenName" :field-type "text")
                                                        (:name "view-sn" :label "sn" :field-type "text")
                                                        (:name "view-mail" :label "mail" :field-type "text")
                                                        (:name "view-postaladdress" :label "postalAddress" :field-type "text")
                                                        (:name "view-postalcode" :label "postalCode" :field-type "text")
                                                        (:name "view-st" :label "st" :field-type "text")
                                                        (:name "view-l" :label "l" :field-type "text")
                                                        (:name "view-telephonenumber" :label "telephoneNumber" :field-type "text")
                                                        (:name "view-mobile" :label "mobile" :field-type "text")
                                                        (:name "view-businesscategory" :label "businessCategory" :field-type "text")
                                                        (:label "Search InetOrg Entries" :field-type "button" :onclick "on_inetorg_view_search_clicked()")))))

(defun inetorg-view-search-json ()
  (with-auth (instance inetorg-view-search-service)
    nil))

(defclass inetorg-view-results (auth-service)
  ((results :initarg :results
            :initform nil
            :accessor results)
   (location-p :initarg :location-p
               :initform nil
               :accessor location-p))
  (:documentation ""))

(defun inetorg-view-results-json (givenname sn mail postaladdress postalcode st l telephonenumber mobile businesscategory)
  (with-auth (instance inetorg-view-results-service)
    (with-ldap (ldap)
      (setf (results instance)
            (sort (search-ldap-users ldap
                                     `((:givenname ,givenname)
                                       (:sn ,sn)
                                       (:mail ,mail)
                                       (:postaladdress ,postaladdress)
                                       (:postalcode ,postalcode)
                                       (:st ,st)
                                       (:l ,l)
                                       (:telephonenumber ,telephonenumber)
                                       (:mobile ,mobile)
                                       (:businesscategory ,businesscategory)))
                  (lambda (x y) (string< (get-cn x) (get-cn y))))))))
