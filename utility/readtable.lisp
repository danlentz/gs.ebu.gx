;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-
;;;;;
;;;;; readtable
;;;;;
;;;;;   

(in-package :gx)

(defun enable-gx-syntax ()
  (set-dispatch-macro-character #\# #\~ #'x:sharp-tilde-reader)
  (set-macro-character  #\< #'gx::double-angle-bracket-reader t)
  (set-macro-character  #\_ #'gx::single-underscore-reader t)
  (set-macro-character  #\" #'rdf::read-string nil))
 
(export '(enable-gx-syntax))


(enable-gx-syntax)

;;;;;
;; Local Variables:
;; indent-tabs: nil
;; outline-regexp: ";;[;]+"
;; End:
;;;;;
