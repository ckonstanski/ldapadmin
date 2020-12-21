;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

(defclass generic-form (base-service)
  ((name :initarg :name
         :initform nil
         :accessor name)
   (http-method :initarg :http-method
                :initform "POST"
                :accessor http-method)
   (action :initarg :action
           :initform nil
           :accessor action)
   (required-p :initarg :required-p
               :initform nil
               :accessor required-p)
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
   (value :initarg :value
          :initform nil
          :accessor value)
   (checked :initarg :checked
            :initform nil
            :accessor checked)
   (field-type :initarg :field-type
               :initform nil
               :accessor field-type)
   (required :initarg :required
             :initform nil
             :accessor required)
   (dismiss :initarg :dismiss
            :initform nil
            :accessor dismiss)
   (options :initarg :options
            :initform nil
            :accessor options)
   (onclick :initarg :onclick
            :initform nil
            :accessor onclick)
   (onchange :initarg :onchange
             :initform nil
             :accessor onchange))
  (:documentation ""))

(defclass option (base-service)
  ((label :initarg :label
          :initform nil
          :accessor label)
   (value :initarg :value
          :initform nil
          :accessor value))
  (:documentation ""))

(defun make-form (name action required-p fields)
  (make-instance 'generic-form
                 :name name
                 :action action
                 :required-p required-p
                 :form-fields (mapcar (lambda (field)
                                        (make-instance 'form-field
                                                       :name (getf field :name)
                                                       :label (getf field :label)
                                                       :value (getf field :value)
                                                       :checked (getf field :checked)
                                                       :field-type (getf field :field-type)
                                                       :required (getf field :required)
                                                       :dismiss (getf field :dismiss)
                                                       :options (mapcar (lambda (option)
                                                                          (make-instance 'option
                                                                                         :label (getf option :label)
                                                                                         :value (getf option :value)))
                                                                        (getf field :options))
                                                       :onclick (getf field :onclick)
                                                       :onchange (getf field :onchange)))
                                      fields)))
