;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

;; ========================================================================== ;;

(defclass generic-form (rest-service)
  ((name :initarg :name
         :initform nil
         :accessor name)
   (http-method :initarg :http-method
                :initform "POST"
                :accessor http-method)
   (action :initarg :action
           :initform nil
           :accessor action)
   (form-fields :initarg :form-fields
                :initform nil
                :accessor form-fields))
  (:documentation ""))

(defclass form-field (base-service)
  ((name :initarg :name
         :initform nil
         :accessor name)
   (label :initarg :label
          :initform nil
          :accessor label)
   (field-type :initarg :field-type
               :initform nil
               :accessor field-type))
  (:documentation ""))

(defmacro define-generic-form-constructor ((form-class name action) fields)
  `(defmethod initialize-instance :after ((,form-class ,form-class) &key)
     (setf (name ,form-class) ,name)
     (setf (action ,form-class) ,action)
     (setf (form-fields ,form-class)
           (mapcar (lambda (form)
                     (make-instance 'form-field
                                    :name (getf form :name)
                                    :label (getf form :label)
                                    :field-type (getf form :field-type)))
                   ,fields))))
