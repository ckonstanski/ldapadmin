;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defpackage :ldapadmin
  (:use :cl :cl-log :hunchentoot)
  (:export :ldapadmin))

(in-package #:ldapadmin)

(defparameter *server-root* (namestring (asdf:system-relative-pathname (intern (package-name *package*)) "./"))
  "The location of the web server root on the filesystem.")

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
             ,@body)))))

(defun logger (output)
  "Logs output to the cl-log log file."
  (log-message :info (format nil "~a: ~a" (net.telent.date:universal-time-to-rfc2822-date (get-universal-time)) output)))

(defun string-to-list (my-string)
  "Converts a sequence to a list \(or whatever the sequence is a
string representation of\)."
  (with-input-from-string (stream my-string)
    (read stream)))

(defmacro add-to-list (output-list &rest value-to-add)
  "Wraps the SETF...APPEND idiom in a smaller package."
  `(setf ,output-list (append ,output-list ,@value-to-add)))

(defun parse-symbol (my-symbol)
  (let ((symbol-parts (ppcre:split "::" (symbol-name my-symbol))))
    (if (= (length symbol-parts) 1)
        (car symbol-parts)
        (cadr symbol-parts))))

(defun flatten (mylist)
  (cond ((atom mylist) mylist)
        ((listp (car mylist))
         (append (flatten (car mylist)) (flatten (cdr mylist))))
        (t (append (list (car mylist)) (flatten (cdr mylist))))))

(defun match-it (regex field)
  "Wraps a PCRE search in a smaller package."
  (cl-ppcre:all-matches-as-strings regex field))

(defun cast-float (string-rep)
  "Tries to return the float representation of `string-rep'.  If
`string-rep' cannot be parsed as a float, returns `nil'."
  (let ((read-value (read-from-string string-rep)))
    (cond ((floatp read-value) read-value)
          ((integerp read-value) (float read-value))
          (t nil))))

(defun pretty-print (raw-string &optional textbox-p)
  "Filters `nil' string values, returning `&nbsp;' instead.  But if
`textbox-p' is t, it returns an empty string instead of `&nbsp;'."
  (let ((trimmed-string (when raw-string (string-trim '(#\Space #\Tab) raw-string))))
    (if (and trimmed-string (> (length trimmed-string) 0))
        (format nil "~a" (ppcre:regex-replace-all "\"" trimmed-string "&quot;"))
        (if textbox-p "" "&nbsp;"))))

(defun strip-milliseconds (sql-datetime)
  (subseq sql-datetime 0 (position #\. sql-datetime)))

#+sbcl
(defun map-slot-names (instance)
  "Returns a list of the names of all the slots of any class instance
using reflection.  The returned values are symbols.  Only works with
SBCL."
  (mapcar #'sb-mop:slot-definition-name
          (sb-mop:class-slots (class-of instance))))

(defun make-document-root-path (document-root relative-path)
  "Makes a relative filesystem path into a full one, using
`document-root' as the base."
  (concatenate 'string document-root relative-path))

(defun make-server-path (relative-path)
  "Makes a relative filesystem path into a full one, using
`*server-root*' as the base."
  (make-document-root-path *server-root* relative-path))

(defun null-or-empty-p (sequence)
  (or (null sequence) (equal (length sequence) 0)))

(defun xml-escape (mystring)
  (setf mystring (ppcre:regex-replace-all "<" mystring "&lt;"))
  (setf mystring (ppcre:regex-replace-all ">" mystring "&gt;"))
  (setf mystring (ppcre:regex-replace-all "&" mystring "&amp;"))
  (setf mystring (ppcre:regex-replace-all "\"" mystring "&quot;"))
  (setf mystring (ppcre:regex-replace-all "'" mystring "&apos;"))
  mystring)

(defun xml-unescape (mystring)
  (setf mystring (ppcre:regex-replace-all "&lt;" mystring "<"))
  (setf mystring (ppcre:regex-replace-all "&gt;" mystring ">"))
  (setf mystring (ppcre:regex-replace-all "&amp;" mystring "&"))
  (setf mystring (ppcre:regex-replace-all "&quot;" mystring "\""))
  (setf mystring (ppcre:regex-replace-all "&apos;" mystring "'"))
  mystring)

(defun trim-last-char (mystring)
  (if (null-or-empty-p mystring)
      ""
      (subseq mystring 0 (- (length mystring) 1))))

(defun string-to-real (string-rep)
  "Converts a string representation of a number to a rational
representation.  For some reason, the lisp community calls rational
numbers `real'. If you pass this method garbage, you will get 0. It
always returns a number."
  (if (null-or-empty-p string-rep)
      0
      (let* ((string-parts (ppcre:split "\\." (string-trim '(#\Space #\Tab) string-rep)))
             (integer-portion (parse-integer (car string-parts) :junk-allowed t))
             (fractional-portion-string (second string-parts))
             (fractional-divisor 1))
        (multiple-value-bind (fractional-portion fractional-portion-length)
            (if (> (length fractional-portion-string) 0)
                (parse-integer fractional-portion-string :junk-allowed t)
                (values nil 0))
          (when (null integer-portion)
            (setf integer-portion 0))
          (when (null fractional-portion)
            (setf fractional-portion 0))
          (when (not (= fractional-portion 0))
            (setf fractional-divisor (expt 10 fractional-portion-length)))
          (if (>= integer-portion 0)
              (+ integer-portion (/ fractional-portion fractional-divisor))
              (- integer-portion (/ fractional-portion fractional-divisor)))))))

(defun real-to-string (real-rep &key (places 2))
  "Converts a rational representaion of a number into a string
representation, rounded to `places' decimal places."
  (when real-rep
    (if (= places 0)
        (format nil "~a" (parse-integer (format nil "~,2f" real-rep) :junk-allowed t))
        (format nil
                (format nil "~~,~af" places)
                (coerce real-rep 'long-float)))))

(defun double-to-real (double-rep &key (places 2))
  "Converts a double to a real.  It does this by converting the double
to a string, and then the string to a real.  Decimal place truncation
happens when converting to a string."
  (string-to-real (real-to-string double-rep :places places)))

(defun pad-with-zeros (string-rep places)
  "Left-pads a string representaion of a number with leading zeros to
make it the specified length."
  (loop for i from (+ (length string-rep) 1) to places do
       (setf string-rep (format nil "0~a" string-rep)))
  string-rep)

(defun shell-wrapper (command)
  "Calls a shell command and returns the output as a list, where each
atom of the list is a string that contains one line of the output."
  (let ((output (make-array '(0) :element-type 'character :fill-pointer 0 :adjustable t)))
    (with-output-to-string (stream output)
      (uffi:run-shell-command command :output stream))
    (loop for line in (ppcre:split #\Newline output)
       collect line)))

(defun reduce-to-char-separated-string (mylist char)
  (format nil "~a" (reduce (lambda (&optional x y)
                             (cond ((and x y) (format nil "~a~a~a" x char y))
                                   ((and x (not y)) (format nil "~a" x))
                                   ((and (not x) y) (format nil "~a" y))
                                   (t "")))
                           mylist)))

(defun reduce-to-comma-separated-string (mylist)
  (reduce-to-char-separated-string mylist ","))

(defun reduce-to-newline-separated-string (mylist)
  (reduce-to-char-separated-string mylist #\Newline))
