;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

(defclass entity ()
  ()
  (:documentation "Superclass for all entity objects. An entity object
is one that represents a single tuple from a data source, like a SQL
table or LDAP record. In this case we're dealing with LDAP records."))

(defmacro with-entity-slots-to-list ((entity slot) &body body)
  "Iterates over all the slots of `entity' and builds an alist based
on those slots. `slot' is the iterator."
  `(remove-if #'null (mapcar (lambda (slot)
                               (when (slot-is-field-p slot)
                                 ,@body))
                             (org-ckons-core::map-slot-names ,entity))))

(defmethod attribute-value-list ((entity entity) &optional (keep-nulls nil))
  (with-entity-slots-to-list (entity slot)
    (when (or keep-nulls
              (and (not keep-nulls)
                   (not (org-ckons-core::null-or-empty-p (slot-value entity slot)))))
      `(,slot . ,(slot-value entity slot)))))

(defmethod get-slots-regex ((entity entity) regex)
  (sort (remove-if-not (lambda (x) (org-ckons-core::match-it regex (symbol-name x)))
                       (org-ckons-core::map-slot-names entity))
        (lambda (x y) (string< (symbol-name x) (symbol-name y)))))

(defmethod intersect-slots ((entity entity) slots)
  (let ((intersect-slots ()))
    (loop for class-slot in (org-ckons-core::map-slot-names entity) do
         (let ((class-slot-string (org-ckons-core::parse-symbol class-slot)))
           (loop for arg-slot in slots do
                (let ((arg-slot-string (org-ckons-core::parse-symbol arg-slot)))
                  (when (string-equal class-slot-string arg-slot-string)
                    (push class-slot intersect-slots))))))
    (nreverse intersect-slots)))

(defun slot-is-field-p (slot)
  (let ((name (symbol-name slot)))
    (and (equal name (ppcre:regex-replace "^\\*" name "")))))
