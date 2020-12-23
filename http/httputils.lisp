;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

(defmacro with-cookie-jar (&rest body)
  `(let ((cookie-jar (make-instance 'drakma:cookie-jar)))
     ,@body))

(defun drakma-request (url
                       cookie-jar
                       &key
                         (method :get)
                         (content-type "application/x-www-form-urlencoded")
                         (content nil)
                         (user-agent :firefox)
                         (redirect t)
                         (auto-referer t)
                         (additional-headers nil)
                         (connection-timeout 20)
                         (proxy nil)
                         (proxy-basic-authorization nil)
                         (basic-authorization nil))
  (drakma:http-request url
                       :cookie-jar cookie-jar
                       :method method
                       :content-type content-type
                       :content content
                       :user-agent user-agent
                       :redirect redirect
                       :auto-referer auto-referer
                       :connection-timeout connection-timeout
                       :additional-headers additional-headers
                       :proxy proxy
                       :proxy-basic-authorization proxy-basic-authorization
                       :basic-authorization basic-authorization
                       :close t))

(defun escape-url (url)
  (let ((escaped-url url))
    (setf escaped-url (ppcre:regex-replace-all "%" escaped-url "%25"))
    (setf escaped-url (ppcre:regex-replace-all "\\?" escaped-url "%3F"))
    (setf escaped-url (ppcre:regex-replace-all "#" escaped-url "%23"))
    (setf escaped-url (ppcre:regex-replace-all "/" escaped-url "%2F"))
    (setf escaped-url (ppcre:regex-replace-all "'" escaped-url "%27"))
    (setf escaped-url (ppcre:regex-replace-all " " escaped-url "%20"))
    (setf escaped-url (ppcre:regex-replace-all "\\(" escaped-url "%28"))
    (setf escaped-url (ppcre:regex-replace-all "\\)" escaped-url "%29"))
    (setf escaped-url (ppcre:regex-replace-all ":" escaped-url "%3A"))
    escaped-url))

(defun unescape-url (url)
  (let ((unescaped-url url))
    (setf unescaped-url (ppcre:regex-replace-all "%25" unescaped-url "%"))
    (setf unescaped-url (ppcre:regex-replace-all "%3F" unescaped-url "?"))
    (setf unescaped-url (ppcre:regex-replace-all "%23" unescaped-url "#"))
    (setf unescaped-url (ppcre:regex-replace-all "%2F" unescaped-url "/"))
    (setf unescaped-url (ppcre:regex-replace-all "%27" unescaped-url "'"))
    (setf unescaped-url (ppcre:regex-replace-all "%20" unescaped-url " "))
    (setf unescaped-url (ppcre:regex-replace-all "%28" unescaped-url "("))
    (setf unescaped-url (ppcre:regex-replace-all "%29" unescaped-url ")"))
    (setf unescaped-url (ppcre:regex-replace-all "%3A" unescaped-url ":"))
    unescaped-url))

(defun newlines-to-backslash-n (body)
  (cl-ppcre:regex-replace-all #\Newline body "\\\\n"))
