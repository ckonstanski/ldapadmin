;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

(defparameter *menu-config* '((:id "a_menu_home" :label "Home" :url "/home" :handler "/home" :permission "t")
                              (:id "a_menu_login" :label "Login" :url "/login" :handler "/login" :permission "anonymous")
                              (:id "a_menu_logout" :label "Logout" :url "/logout" :handler "/logout" :permission "admin")
                              (:id "a_menu_inetorg_view" :label "View InetOrg Entries" :url "/inetorg/view" :handler "/inetorg/view" :permission "admin")
                              (:id "a_menu_inetorg_add" :label "Add InetOrg Entry" :url "/inetorg/add" :handler "/inetorg/add" :permission "admin")))

(defclass menuitem ()
  ((id :initarg :id
       :initform nil
       :accessor id)
   (label :initarg :label
          :initform nil
          :accessor label)
   (url :initarg :url
        :initform nil
        :accessor url)
   (handler :initarg :handler
            :initform nil
            :accessor handler)
   (permissions :initarg :permissions
                :initform nil
                :accessor permissions)
   (children :initarg :children
             :initform nil
             :accessor children))
  (:documentation ""))
  
(defclass menu-service (base-service)
  ((menuitems :initarg :menuitems
              :initform nil
              :accessor menuitems))
  (:documentation ""))

(defmethod initialize-instance :after ((menu-service menu-service) &key)
  (setf (menuitems menu-service)
        (mapcar (lambda (x)
                  (make-instance 'menuitem
                                 :id (getf x :id)
                                 :label (getf x :label)
                                 :url (getf x :url)
                                 :handler (getf x :handler)))
                (remove-if 'null (mapcar (lambda (x)
                                           (when (find-if (lambda (y)
                                                            (string= (getf x :permission) y))
                                                          `("t" ,(session-value :permissions)))
                                             x))
                                         *menu-config*)))))

(defun menu-json ()
  (objects-to-json `(,(make-instance 'menu-service))))
