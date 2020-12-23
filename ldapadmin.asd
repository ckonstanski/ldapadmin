;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:cl)

(defpackage #:ldapadmin-system (:use #:cl #:asdf))
(in-package #:ldapadmin-system)

(defmacro do-defsystem (&key name version maintainer author description long-description depends-on components)
  `(defsystem ,name
       :name ,name
       :version ,version
       :maintainer ,maintainer
       :author ,author
       :description ,description
       :long-description ,long-description
       :depends-on ,(eval depends-on)
       :components ,components))

(defparameter *quicklisp-packages* '(net-telent-date cl-ppcre uffi hunchentoot cl-log ironclad cl-json drakma trivial-ldap))
(defparameter *asdf-packages* '(org-ckons-core))
(defparameter *all-packages* (append *quicklisp-packages* *asdf-packages*))

(loop for pkg in *quicklisp-packages* do
     (ql:quickload (symbol-name pkg)))

(do-defsystem :name "ldapadmin"
              :version "2"
              :maintainer "Carlos Konstanski <me@ckons.org>"
              :author "Carlos Konstanski <me@ckons.org>"
              :description "ldapadmin"
              :long-description "ldapadmin is a web application written in Common Lisp, based on the Hunchentoot web server. It is an LDAP frontend web UI."
              :depends-on *all-packages*
              :components ((:module core
                            :components ((:file "core")))
                           (:module condition
                            :depends-on (core)
                            :components ((:file "condition")))
                           (:module http
                            :depends-on (condition)
                            :components ((:file "httputils")
                                         (:file "html")))
                           (:module file
                            :depends-on (condition)
                            :components ((:file "file-utils")))
                           (:module json
                            :depends-on (condition)
                            :components ((:file "json-utils")))
                           (:module ldap
                            :depends-on (json)
                            :components ((:file "generics")
                                         (:file "ldap" :depends-on ("generics"))))
                           (:module entity
                            :depends-on (ldap)
                            :components ((:file "generics")
                                         (:file "entity" :depends-on ("generics"))
                                         (:file "ldap-user" :depends-on ("entity"))))
                           (:module service
                            :depends-on (entity)
                            :components ((:file "base-service")
                                         (:file "rest-service" :depends-on ("base-service"))
                                         (:file "auth-service" :depends-on ("rest-service"))
                                         (:file "generic-form" :depends-on ("rest-service"))
                                         (:file "menu-service" :depends-on ("base-service"))
                                         (:file "home-service" :depends-on ("rest-service"))
                                         (:file "login-service" :depends-on ("generic-form"))
                                         (:file "logout-service" :depends-on ("rest-service"))
                                         (:file "inetorg-view-service" :depends-on ("auth-service" "generic-form"))
                                         (:file "inetorg-modify-service" :depends-on ("auth-service" "generic-form"))
                                         (:file "inetorg-delete-service" :depends-on ("auth-service" "generic-form"))
                                         (:file "inetorg-add-service" :depends-on ("auth-service" "generic-form"))))
                           (:module webapps
                            :depends-on (service)
                            :components ((:file "webapp-loader")
                                         (:module ldapadmin
                                          :depends-on ("webapp-loader")
                                          :components ((:file "site")))))))
