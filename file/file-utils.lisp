;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

;; ========================================================================== ;;

(defun compile-and-load (filename)
  "Compiles and then loads a file.  `filename' should not have an
extension, such as .fasl or .lisp."
  (compile-file filename)
  (load filename))

(defun write-pid-file ()
  (shell-wrapper (format nil "echo ~a >~a/~a.pid" (sb-posix:getpid) (sb-posix:getenv "HOME") (string-downcase (package-name *package*)))))

(defun file-to-list (infile)
  "Reads `infile' and returns a list, where each atom is a single
line of the file.  `infile' can be a string or a pathname
object."
  (let ((infile-list ()))
    (with-open-file (filehandle infile :if-does-not-exist nil)
      (if (streamp filehandle)
          (progn
            (loop for line = (read-line filehandle nil)
               while line do
                 (push line infile-list))
            (nreverse infile-list))
          nil))))

(defun parse-csv-line (line)
  "Parses a single line of CSV input into a list of string fields."
  (let ((quoted-string-mode nil)
        (line-list ())
        (field-collector ""))
    (loop for this-char across (ppcre:regex-replace-all #\Return line "") do
         (cond ((equal this-char #\")
                (setf quoted-string-mode (not quoted-string-mode)))
               ((and (equal this-char #\,) (not quoted-string-mode))
                (push field-collector line-list)
                (setf field-collector ""))
               (t
                (setf field-collector (format nil "~a~a" field-collector this-char)))))
    ;; get the field after the last comma
    (push field-collector line-list)
    (nreverse line-list)))

(defmacro dofile ((line filename) &body body)
  "Wrapper for the common task of opening a file and reading it one
line at a time."
  (let ((stream (gensym)))
    `(with-open-file (,stream ,filename)
       (loop for ,line = (read-line ,stream nil nil)
          while ,line do ,@body))))

(defun mkdir (path)
  (uffi:run-shell-command (format nil "mkdir -p ~a" path)))

(defun copy-file (source destination)
  "Copies file from `source' to `destination'.  If the destination
directory does not exist, it will be created."
  (when (and (file-exists-p source)
             (file-p source)
             (not (null-or-empty-p destination)))
    (let ((destination-parts (nreverse (remove-if #'null-or-empty-p (ppcre:split "/" destination))))
          (destination-file "")
          (destination-directory ""))
        (setf destination-file (pop destination-parts))
        (loop for part in (nreverse destination-parts) do
             (setf destination-directory (format nil "~a/~a" destination-directory part)))
        (mkdir destination-directory)
        (uffi:run-shell-command (format nil "cp '~a' '~a'" source destination)))))

(defun purge-old-files (directory-path)
  "Deletes everything out of a directory that is older than 8 hours
old."
  (uffi:run-shell-command (format nil "find ~a/* -mmin 480 |xargs rm -rf" directory-path)))

(defun purge-files-regex (directory-path regex)
  "Deletes everything out of a directory whose name matches the
`regex'."
  (uffi:run-shell-command (format nil "find ~a/* -regex '~a' |xargs rm -rf" directory-path regex)))

(defun file-exists-p (file-path)
  (equal (car (shell-wrapper (format nil "if test -f '~a'; then echo 0; else echo 1; fi" file-path))) "0"))

(defun file-mtime (file-path)
  (let ((output (car (shell-wrapper (format nil "ls --full-time '~a' |awk '{ print $6,$7 }' |awk -F. '{ print $1; }'" file-path)))))
    (if (not (match-it "^\\d\\d\\d\\d-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d$" output))
        (error 'handled-error :text (format nil "Error in `file-mtime': ~a" output))
        output)))

(defun directory-p (absolute-path)
  (if (car (shell-wrapper (format nil "file '~a' |grep 'directory'" absolute-path))) t nil))

(defun symlink-p (absolute-path)
  (if (car (shell-wrapper (format nil "file '~a' |grep 'symbolic link'" absolute-path))) t nil))

(defun file-p (absolute-path)
  (if (or (directory-p absolute-path) (symlink-p absolute-path)) nil t))

(defun path-contained-p (root-path path-to-check)
  "Returns `(,root-path) if `path-to-check' is contained within `root-path',
`nil' otherwise."
  (match-it root-path path-to-check))

(defun find-files (working-dir base-dir pattern)
  (shell-wrapper (format nil
                         "pushd ~a >/dev/null 2>&1 ; find ~a -iname '~a' ; popd >/dev/null 2>&1"
                         working-dir
                         base-dir
                         pattern)))
