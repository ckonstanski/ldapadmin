;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

(defclass home-service (rest-service)
  ((content :initarg :content
            :initform nil
            :accessor content))
  (:documentation ""))

(defmethod initialize-instance :after ((home-service home-service) &key)
  (setf (content home-service) (format nil "Welcome to the ~a website" (title *webapp*))))

(defun home-json (&optional message errormsg)
  (objects-to-json `(,(make-instance 'home-service
                                     :message message
                                     :errormsg errormsg))))
