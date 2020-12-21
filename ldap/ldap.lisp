;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

(defclass ldap () 
  ((ldap-host :initarg :ldap-host
              :initform nil
              :accessor ldap-host)
   (sslflag :initarg :sslflag
            :initform nil
            :accessor sslflag)
   (username :initarg :username
             :initform nil
             :accessor username)
   (password :initarg :password
             :initform nil
             :accessor password)
   (base-dn :initarg :base-dn
            :initform nil
            :accessor base-dn)
   (debug-mode :initarg :debug-mode
               :initform nil
               :accessor debug-mode)
   (config :initarg :config
           :initform nil
           :accessor config)
   (connection :initarg :connection
               :initform nil
               :accessor connection))
  (:documentation "Used to provide an object-oriented interface to the
LDAP options in the webapp config file."))

(defmethod initialize-instance :after ((ldap ldap) &key config)
  "Parses the LDAP config from the options.lisp file into a new `ldap'
object."
  (let ((conf (car config)))
    (setf (ldap-host ldap) (getf conf :ldap-host))
    (setf (sslflag ldap) (getf conf :sslflag))
    (setf (username ldap) (getf conf :username))
    (setf (password ldap) (getf conf :password))
    (setf (base-dn ldap) (getf conf :base-dn))
    (setf (debug-mode ldap) (getf conf :debug-mode))
    (setf (config ldap) conf)
    (setf (connection ldap) (apply #'ldap:new-ldap
                                   `(:host ,(ldap-host ldap)
                                     :sslflag ,(sslflag ldap)
                                     :user ,(username ldap)
                                     :pass ,(password ldap)
                                     :base ,(base-dn ldap)
                                     :reuse-connection ,'ldap:rebind
                                     :debug ,(debug-mode ldap))))))

(defmethod disconnect ((ldap ldap))
  (ldap:unbind (connection ldap)))

(defmacro with-ldap ((ldap-name) &body body)
  "Convenience macro for instantiating an `ldap' instance and using it
in an `unwind-protect'."
  `(let ((,ldap-name (make-instance 'ldap :config `(,(ldap *webapp*)))))
     (unwind-protect
          (progn
            ,@body)
       (disconnect ,ldap-name))))

(defmacro with-ldap-iterate ((ldap-entry search-base) &body body)
  "Runs an ldapsearch and executes `body' over each result. The
variable `ldap-entry' is bound to the iterator of the
`ldap:dosearch'. You may use it in your `body'."
  `(progn
     (ldap:bind (connection ldap))
     (ldap:dosearch (,ldap-entry (ldap:search (connection ldap) ,search-base))
       ,@body)))

(defmethod get-ldap-users ((ldap ldap) search-base)
  (let ((ldap-users ()))
    (with-ldap-iterate (ldap-entry search-base)
        (let ((ldap-user (populate-ldap-user ldap-entry)))
          (push ldap-user ldap-users)))
    (nreverse ldap-users)))

(defmethod search-ldap-users ((ldap ldap) search-terms)
  (get-ldap-users ldap (format nil
                               "(&(objectclass=inetOrgPerson)(!(cn=Manager))(!(uid=root))(!(uid=nobody))~a)"
                               (build-search-base search-terms))))

(defmethod get-ldap-user ((ldap ldap) cn)
  (car (search-ldap-users ldap `((:cn ,cn)))))

(defun check-ldap-password (config dn password)
  "Uses ldapwhoami to check the userPassword of a given
binddn. Returns `t' if the password is valid, `nil' otherwise."
  (= 0 (uffi:run-shell-command (format nil
                                       "ldapwhoami -x -H ~a://~a -D 'cn=~a,~a' -w ~a"
                                       (if (getf config :sslflag) "ldaps" "ldap")
                                       (getf config :ldap-host)
                                       dn
                                       (getf config :base-dn)
                                       password))))

(defun populate-ldap-user (ldap-entry)
  "Copies the data from a single LDAP entry as produced by
`with-ldap-iterate' into a new `ldap-user' object."
  (let* ((ldap-user (make-instance 'ldap-user))
         (valid-attribute-names (intersect-slots ldap-user (mapcar (lambda (pair)
                                                                     (car pair))
                                                                   (ldap:attrs ldap-entry)))))
    (loop for name-value in (ldap:attrs ldap-entry) do
         (let ((name (intern (symbol-name (car name-value)) (find-package (string-upcase "ldapadmin"))))
               (value (cadr name-value)))
           (when (find name
                       valid-attribute-names
                       :test (lambda (x y)
                               (string-equal (symbol-name x) (symbol-name y))))
             (setf (slot-value ldap-user name) value))))
    ldap-user))

(defun build-search-base (search-terms)
  "Converts a plist like `((:givenname \"Carlos\") (:sn
\"Konstanski\"))' to an LDAP search base fragment like
\"(givenname=Carlos)(sn=Konstanski)\". Any `nil' or empty-string
values are ignored."
  (reduce-to-char-separated-string (mapcar (lambda (term)
                                             (when (not (null-or-empty-p (cadr term)))
                                               (format nil
                                                       "(~a=~a)"
                                                       (symbol-name (car term))
                                                       (cadr term))))
                                           search-terms)
                                   ""))
