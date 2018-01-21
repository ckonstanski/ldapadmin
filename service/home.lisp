;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

;; ========================================================================== ;;

(defclass home (rest-service)
  ((content :initarg :content
            :initform nil
            :accessor content))
  (:documentation ""))

(defmethod initialize-instance :after ((home home) &key)
  (setf (content home) (concatenate 'string "Welcome to the " (title *webapp*))))

(defun home-json ()
  (objects-to-json `(,(make-instance 'home))))
