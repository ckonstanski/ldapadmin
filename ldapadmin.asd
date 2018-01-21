;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:cl)

;; ========================================================================== ;;

(defpackage #:ldapadmin-system (:use #:cl #:asdf))
(in-package #:ldapadmin-system)

;; ========================================================================== ;;

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

;; ========================================================================== ;;

(defparameter *asdf-packages* '(net-telent-date cl-ppcre uffi hunchentoot cl-log ironclad cl-json drakma trivial-ldap))

;; ========================================================================== ;;

(loop for pkg in *asdf-packages* do
     (ql:quickload (symbol-name pkg)))

;; ========================================================================== ;;

(do-defsystem :name "ldapadmin"
              :version "1.00.000"
              :maintainer "Carlos Konstanski <ckonstanski@pippiandcarlos.com>"
              :author "Carlos Konstanski <ckonstanski@pippiandcarlos.com>"
              :description "ldapadmin"
              :long-description "ldapadmin is a web application written in Common Lisp, based on the Hunchentoot web server. Its purpose is to be an administrative frontend to an openldap server."
              :depends-on *asdf-packages*
              :components ((:module core
                            :components ((:file "coreutils")
                                         (:file "httputils" :depends-on ("coreutils"))
                                         (:file "html" :depends-on ("coreutils"))))
                           (:module condition
                            :depends-on (core)
                            :components ((:file "condition")))
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
                                         (:file "menu" :depends-on ("base-service"))
                                         (:file "home" :depends-on ("rest-service"))
                                         (:file "login" :depends-on ("generic-form"))
                                         (:file "login-authenticate" :depends-on ("rest-service"))
                                         (:file "logout" :depends-on ("rest-service"))
                                         (:file "inetorg-view" :depends-on ("auth-service" "generic-form"))
                                         (:file "inetorg-modify" :depends-on ("auth-service" "generic-form"))
                                         (:file "inetorg-delete" :depends-on ("auth-service" "generic-form"))
                                         (:file "inetorg-add" :depends-on ("auth-service" "generic-form"))))
                           (:module webapps
                            :depends-on (service)
                            :components ((:file "webapp-loader")
                                         (:module ldapadmin
                                          :depends-on ("webapp-loader")
                                          :components ((:file "site")))))))
