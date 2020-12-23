;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defpackage :ldapadmin
  (:use :cl :cl-log :hunchentoot)
  (:export :ldapadmin))

(in-package #:ldapadmin)
