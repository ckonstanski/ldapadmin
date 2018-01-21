;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

;; ========================================================================== ;;

(defclass rest-service (base-service)
  ((location :initarg :location
             :initform nil
             :accessor location)
   (location-p :initarg :location-p
               :initform t
               :accessor location-p)
   (errormsg :initarg :errormsg
             :initform nil
             :accessor errormsg)
   (message :initarg :message
            :initform nil
            :accessor message))
  (:documentation ""))

(defmethod initialize-instance :after ((rest-service rest-service) &key)
  (when (and (location-p rest-service) (null (location rest-service)))
    (setf (location rest-service) (type-to-path rest-service))
    (setf (session-value :location) (location rest-service))))

(defun location-json ()
  (let ((location (if (session-value :location) (session-value :location) "/home")))
  (format nil "{\"location\":\"~a\"}" location)))

(defun type-to-path (rest-type)
  (concatenate 'string "/" (ppcre:regex-replace "-" (string-downcase (type-of rest-type)) "/")))
