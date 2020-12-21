;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

(defvar *acceptor* nil)
(defvar *dispatch-table* '(#'dispatch-easy-handlers #'default-dispatcher))
(defvar *webapps* (make-hash-table :test 'equal))
(defvar *webapp* nil)
(defparameter *port* 3006)
(defparameter *session-timeout* 14400)

(defclass webapp ()
  ((name :initarg :name
         :initform nil
         :accessor name
         :documentation "The name of the webapp as used in the code. A
string used as the key to any webapp config lookup.")
   (url :initarg :url
        :initform nil
        :accessor url
        :documentation "The domain portion of the URL to the
root of the webapp.")
   (document-root :initarg :document-root
                  :initform nil
                  :accessor document-root
                  :documentation "The absolute filesystem path to
the webapp's top-level directory, which is inside the webapps
folder.")
   (title :initarg :title
          :initform nil
          :accessor title
          :documentation "The default title that shows up in
the browser title bar.")
   (meta-description :initarg :meta-description
          :initform nil
          :accessor meta-description
          :documentation "The text that goes into the META DESCRIPTION
tag, and anywhere else we want to put this text so that it will show
up in Google.")
   (ldap :initarg :ldap
         :initform nil
         :accessor ldap))
  (:documentation ""))

(defgeneric get-site-file-path (webapp)
  (:documentation "Builds a full filesystem path to a webapp's site
file."))

(defmethod get-site-file-path ((webapp webapp))
  (format nil "~a/site" (document-root webapp)))

(defgeneric get-pages-file-paths (webapp)
  (:documentation ""))

(defmethod get-pages-file-paths ((webapp webapp))
  (mapcar (lambda (pages-file)
            (ppcre:regex-replace-all "\\.lisp$" (format nil "~a" pages-file) ""))
          (remove-if (lambda (x) (equal x "shared"))
                     (shell-wrapper (format nil "find '~a' -maxdepth 1 -type f -iname 'pages*.lisp' |sort" (document-root webapp))))))

(defun make-webapp-path (relative-path)
  "Makes an absolute filesystem path to a location in the webapps
folder."
  (concatenate 'string *server-root* "webapps/" relative-path))

(defun get-options-files ()
  (mapcar (lambda (webapp-directory)
            (format nil "~a/conf/options.lisp" webapp-directory))
          (remove-if (lambda (x) (or (match-it "webapps/$" x)
                                     (match-it "webapps/shared$" x)
                                     (match-it "webapps/CVS$" x)
                                     (match-it "webapps/\\.$" x)
                                     (match-it "webapps/\\.\\.$" x)))
                     (shell-wrapper (format nil "find '~a' -maxdepth 1 -type d |sort" (make-webapp-path ""))))))

(defun set-webapp (webapp)
  "Sets a `webapp' object in `*webapps*'.  The lookup key is the
webapp name.  If a webapp already exists under this key, it gets
overwritten with the new one."
  (setf (gethash (name webapp) *webapps*) webapp))

(defun get-webapp (key)
  "Gets the webapp object."
  (gethash key *webapps*))

(defun generate-sessionid ()
  "Generates a unique random string to seed the
`*session-secret*'. The string is a SHA256 hash."
  (let ((entropic-value (make-array '(32) :element-type '(unsigned-byte 8))))
    (with-open-file (urandom-file "/dev/urandom" :direction :input :element-type '(unsigned-byte 8))
      (loop for i from 0 to 31 do
           (setf (elt entropic-value i) (read-byte urandom-file))))
    (let ((digest (ironclad:make-digest 'ironclad:sha256)))
      (ironclad:update-digest digest entropic-value)
      (ironclad:byte-array-to-hex-string (ironclad:produce-digest digest)))))

(defun populate-webapps ()
  (loop for options-file in (get-options-files) do
       (with-open-file (input options-file :direction :input)
         (let* ((form (car (read input))))
           (set-webapp (make-instance 'webapp
                                      :name (getf form :name)
                                      :url (getf form :url)
                                      :document-root (make-webapp-path (getf form :document-root))
                                      :title (getf form :title)
                                      :meta-description (getf form :meta-description)
                                      :ldap (getf form :ldap)))))))

(defun ldapadmin ()
  "Call this to start the server."
  (when (null *acceptor*)
    (let ((package (string-downcase (package-name *package*))))
      (populate-webapps)
      (setf (log-manager) (make-instance 'log-manager :message-class 'formatted-message))
      (start-messenger 'text-file-messenger :filename (format nil "/var/log/lisp/~a.log" package))
      (setf *session-secret* (generate-sessionid))
      (populate-webapps)
      (setf *acceptor* (start (make-instance 'easy-acceptor
                                             :port *port*
                                             :document-root (make-server-path (format nil "webapps/~a/" package))
                                             :name (format nil "~a-acceptor" package)))))))

(defmacro with-request-wrapper (uri page-function)
  ;; Assigning package outside the backquote is necessary because
  ;; *package* resolves incorrectly to common-lisp-user inside the
  ;; backquote.
  (let ((package (string-downcase (package-name *package*))))
    `(let ((*webapp* (get-webapp ,package)))
       (logger (format nil "Page request URI: [~a]" ,uri))
       (unless *session*
         (start-session)
         (setf (session-max-time *session*) *session-timeout*)
         (setf (session-value :permissions) "anonymous"))
       (,page-function))))
  
(defmacro define-endpoint (request-type uri var-list page-function)
  "Does the grunt work of creating an `easy-handler' for each page you
wish to publish."
  (let ((name (gensym)))
    `(progn
       (logger (format nil "Publishing page. URL = [~a]" ,uri))
       (define-easy-handler (,name :uri ,uri :default-request-type ,request-type)
           ,var-list
         (with-request-wrapper ,uri ,page-function)))))
