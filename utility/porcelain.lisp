;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-
;;;;;
;;;;; porcelain.lisp
;;;;; 
;;;;; Author:      Dan Lentz, Lentz Intergalactic Softworks, Tue Jan 25 22:37:21 2011
;;;;; Maintainer:  <danlentz@me.com>
;;;;;
;;;;; I just heard the SEVENTIES were over!!  And I was just getting in touch
;;;;; with my LEISURE SUIT!!
;;;;;

(defpackage :gx (:use :cl)
  (:export
    #:of
    #:all
  ;;  #:in
    #:all*
    #:has
    #:has*
    #:in-ns
    #:all-ns
    #:ie))

(in-package :gx)

(defun of (resource)
  (get-slots resource))

(defun in-ns (ns)
  (list-all-entities-in ns))

(defun all (class)
  (collect-direct-instances-of class))

(defun all* (class)
  (collect-all-instances-of class))

(defun has (property)
  (get-domain property))

(defun has* (property)
  (collect-all-extensions-of property))

(defun all-ns ()
  (list-all-uri-namedspaces))

(defun ie (thing)
  (swank:inspect-in-emacs thing))

;;;;;
;; Local Variables:
;; indent-tabs: nil
;; outline-regexp: ";;[;]+"
;; End:
;;;;;
