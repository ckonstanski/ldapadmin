;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

;; ========================================================================== ;;

(defun json-to-object (object-type json-obj)
  (remove-if-not (lambda (x)
                   (let ((found-value nil))
                     (loop for slot in (map-slot-names x) do
                          (when (slot-value x slot)
                            (setf found-value t)))
                     found-value))
                 (mapcar (lambda (obj)
                           (let ((object (make-instance object-type)))
                             (loop for slot in (map-slot-names object) do
                                  (let ((symb (intern (symbol-name slot) :keyword)))
                                    (setf (slot-value object slot)
                                          (cdr (find-if (lambda (param) (eq (car param) symb)) obj)))))
                             object))
                         json-obj)))

;; ========================================================================== ;;

(defun objects-to-json (list-of-objects &optional (explicit-encoder-p nil))
  (labels ((objectp (object)
             (not (eq () (remove-if 'null (mapcar (lambda (superclass)
                                                    (eq (class-name superclass) 'base-service))
                                                  (sb-mop:class-direct-superclasses (class-of object)))))))
           (list-of-lists-p (object)
             (and (listp object)
                  (find-if (lambda (y) (not (null y)))
                           (mapcar (lambda (x)
                                     (listp x))
                                   object))))
           (list-of-objects-p (object)
             (and (listp object)
                  (find-if (lambda (y) (not (null y)))
                           (mapcar (lambda (x)
                                     (objectp x))
                                   object))))
           (map-slots (object)
             (remove-if 'null (mapcar (lambda (slot)
                                        (let ((symb (intern (symbol-name slot) :keyword))
                                              (value (slot-value object slot)))
                                          (when value
                                            (cons symb (cond ((or (equal value (json:json-bool t))
                                                                  (equal value (json:json-bool nil)))
                                                              value)
                                                             ((list-of-lists-p value)
                                                              (listify value))
                                                             ((list-of-objects-p value)
                                                              (if explicit-encoder-p
                                                                  (error "Can't do list-of-objects with the explicit encoder.")
                                                                  (listify value)))
                                                             ((listp value)
                                                              (map 'vector #'identity value))
                                                             ((objectp value)
                                                              (if explicit-encoder-p
                                                                  (cons :object (map-slots value))
                                                                  (map-slots value)))
                                                             (t
                                                              value))))))
                                      (map-slot-names object))))
           (listify (list-of-objects)
             (mapcar (lambda (object)
                       (map-slots object))
                     list-of-objects)))
    (let ((listobj (listify list-of-objects)))
      (reduce-to-comma-separated-string (mapcar (lambda (alist)
                                                  (if explicit-encoder-p
                                                      (json:with-explicit-encoder
                                                        (json:encode-json-to-string (cons :object alist)))
                                                      (json:encode-json-alist-to-string alist)))
                                                listobj)))))