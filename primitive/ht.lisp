;;; ht.lisp --- hash table utils
;; -*- mode: lisp; syntax: common-lisp; coding: mule-utf-8; -*-

;; Copyright (C) 2008 Dan Lentz

;; Version: $Rev$
;; Created: by dan@lentz.com at 2008/12/03 11:54:45
;; Maintainer: $LastChangedBy$
;; X-SVN-URL: $URL$
;; X-SVN-Updated: $Date$
;; X-SVN-Id: $Id$
;; Keywords: lisp 

;;; Code:

(defpackage :x
	(:use :cl))
	
(in-package :x)

(defun ht-print (hash-table)
  (let (previous)
    (pprint-logical-block (nil nil :prefix "(" :suffix ")")
      (maphash (lambda (key value)
                 (pprint-pop)
                 (if previous
                     (write-char #\Space)
                     (setf previous t))
                 (write key)
                 (write-char #\Space)
                 (write value)
                 (pprint-newline :fill))
               hash-table))))

(defun ht-nonempty (ht)
  (maphash #'(lambda (k v) (return-from ht-nonempty t))
           ht)
  nil)

(defun ht-car (ht)
  (maphash #'(lambda (k v) (return-from ht-car v))
           ht))

(defun ht-keys (ht)
  (let ((acc nil))
    (maphash #'(lambda (k v) 
                 (declare (ignore v)) 
                 (push k acc))
             ht)
    acc))

(defun ht-vals (ht)
  (let ((acc nil))
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (push v acc))
             ht)
    acc))

(defun ht-pairs (ht)
  (let ((acc nil))
    (maphash #'(lambda (k v)
                 (push (cons k v) acc))
             ht)
    acc))

(defun somehash (fn ht)
  (maphash #'(lambda (k v)
               (when (funcall fn v)
                 (return-from somehash v)))
           ht)
  nil)
     
(defun key-match (ht1 ht2)
  (maphash #'(lambda (k v)
               (declare (ignore v))
               (when (gethash k ht2)
                 (return-from key-match k)))
           ht1)
  nil)

#+nil
(defun print-ht (ht str)
  (maphash #'(lambda (k v)
               (print (cons k v) str))
           ht))

(defun ht-read (ht str)
  (do-stream pair str
    (setf (gethash (car pair) ht)
          (cdr pair)))
  ht)



;; Local Variables:
;;  tab-width: 8
;;  fill-column: 78
;;  indent-tabs-mode: nil
;;  comment-column: 52
;;  comment-start: ";; "
;; End:
;;; ht.lisp ends here