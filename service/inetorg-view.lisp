;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

;; ========================================================================== ;;
;; inetorg-view

(defclass inetorg-view (auth-service)
  ((instructions :initarg :instructions
                 :initform nil
                 :accessor instructions))
  (:documentation ""))

(defun inetorg-view-json ()
  (with-auth (instance inetorg-view)
    (setf (instructions instance) "Use the form to filter the InetOrg results.")))

;; ========================================================================== ;;
;; inetorg-view-search

(defclass inetorg-view-search (auth-service generic-form)
  ((location-p :initarg :location-p
               :initform nil
               :accessor location-p))
  (:documentation ""))

(define-generic-form-constructor (inetorg-view-search "inetorg-view-search-form" "/inetorg/view/results")
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
      (:name "view-submit" :label "Search InetOrg Entries" :field-type "button")))

(defun inetorg-view-search-json ()
  (with-auth (instance inetorg-view-search)
    nil))

;; ========================================================================== ;;
;; inetorg-view-results

(defclass inetorg-view-results (auth-service)
  ((results :initarg :results
            :initform nil
            :accessor results)
   (location-p :initarg :location-p
               :initform nil
               :accessor location-p))
  (:documentation ""))

(defun inetorg-view-results-json (givenname sn mail postaladdress postalcode st l telephonenumber mobile businesscategory)
  (with-auth (instance inetorg-view-results)
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
