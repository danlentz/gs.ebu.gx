;;;-*- Mode: common-lisp; syntax: common-lisp; package: asdf; base: 10 -*-
;;;
;;;; SWCLOS: A Semantic Web Processor on CLOS
;;;
;;; IT Program Project in Japan: 
;;:    Building Operation-Support System for Large-scale System using IT.
;;;
;;; This code is written by Seiji Koide at Galaxy Express Corporation, Japan,
;;; for the realization of the MEXT IT Program in Japan.
;;;
;;; Copyright � 2003, 2004, 2006 by Galaxy Express Corporation
;;; 
;;; Copyright (c) 2007, 2008, 2009 Seiji Koide

;;; ASDF system definition.
;;; This file must be used without compiling.

(defpackage gx-system (:use :common-lisp :asdf))  
 
(in-package :gx-system)

(defparameter asdf:*asdf-verbose* nil)
(defparameter *compile-verbose* nil)

;; experimental
(let ((loadpath *load-truename*))
  (defun gx-directory ()
    (make-pathname :directory (pathname-directory loadpath))))

(if (not (excl::logical-host-p "CODE"))
    (error "System :GX requires declaration of dominating logical host: CODE")  
    (setf (logical-pathname-translations "GX") `(("**;*.*"          "CODE:gx;**;*.*")
                                                 ("ontology;**;*.*" "CODE:gx;ontology;**;*.*")
                                                 ("rdf;**;*.*"      "CODE:gx;rdf;**;*.*")
                                                 ("rdfs;**;*.*"     "CODE:gx;rdfs;**;*.*")
                                                 ("owl;**;*.*"      "CODE:gx;owl;**;*.*")
                                                 ("ntriple;**;*.*"  "CODE:gx;ntriple;**;*.*"))))

(defsystem :GX
  
  :name        "Galaxy Express" :version "0.9.2" :licence "SWCLOS"
  :description "SWCLOS implements OWL Full sematics for CLOS metaobject protcol."
  :author      "Seiji Koide <SeijiKoide@aol.com>"
  :maintainer  "Dan Lentz <danlentz@gmail.com>"

  :long-description "Every resource in RDF and RDF(S), e.g.,
    rdfs:Class, rdfs:Resource, rdf:Property, and resource instances
    and properties are realized as CLOS objects with straightforward
    mapping RDF(S) classes/instances to CLOS classes/instances. Axioms
    and entailment rules in RDF(S) and OWL are embodied in the system
    so that a lisp programmer can make ontology in RDF(S) and use the
    ontology within the semantics specified by RDF(S) documents.

    OWL semantics are implemented on top of RDF(S) as the extension and
    augmentation of RDF(S) semantics. In SWCLOS, every instance of
    owl:Thing is an instance of rdfs:Resource, and every class of
    owl:Class is also a class of rdfs:Class. Therefore, any rule or
    method in RDF(S) works in OWL."
  
  :depends-on  (:logv :iterate)
;;  :do-first    (lambda () (require :cg))
  :properties  (:debug t :persistence nil :log-to #p"GX:log;message.log")
  :components

  ((:static-file "gx.asd") (:file "packages")

    (:module PRIMITIVE :depends-on ("packages")

      :serial t
      :components ((:file "log"  :version "0.2.0"     :properties
                     (:author "Nick Levine <ndl@ravenbrook.com>"))
                    (:file "time" :version "0.0.1"    :properties
                      (:author "unknown"))
                    (:file "as"   :version "0.0.1"    :properties
                      (:author "Mikel Evins"))
                    (:file "ns"   :version "0.1.1"    :properties
                      (:author "Michael Weber <michaelw@foldr.org>"))
                    (:file "def"  :version "0.1.5"    :properties
                      (:author "Volkan Yazici <volkan.yazici@gmail.com>"))
                    (:file "a"    :version "0.0.1"    :properties
                      (:author "Marco Antoniotti"))
                    (:file "collect" :version "0.0.1" :properties
                      (:author "unknown"))
                    (:file "trie" :version "0.0.1"    :properties
                      (:author "unknown"))
                    (:file "seq"  :version "0.3.1"    :properties
                      (:author "Kevin M. Rosenberg"))
                    (:file "dtrace" :version "0.0.1"  :properties
                      (:author "unknown"))
                    (:file "stream" :version "0.0.1"  :properties
                      (:author "unknown"))
                    (:file "ht" :version "0.0.1"      :properties
                      (:author "unknown"))
                    (:file "defmacro-star")
                    (:file "readtable")
                    (:file "tree" :version "0.0.5"    :properties
                      (:author "Pascal J. Bourguignon <pjb@informatimago.com>"))))
    
    (:module RDF :depends-on ("packages")
      :serial t
      :components ((:file "utils")
                    (:file "rdf-io")
                    (:file "xml")
                    (:file "rdf-error"       :depends-on ("utils"))
                    (:file "namespace")
                    (:file "rdf-share"       :depends-on ("rdf-io" "namespace"))
                    (:file "rdf"             :depends-on ("namespace" "rdf-share"))
                    (:file "rdf-reader")))
    
    (:module RDFS :depends-on (RDF)     
      :serial t
      :components ((:file "slot-def")
                    (:file "rdf-boot");;           :depends-on ("SlotDef"))
                    (:file "gx-type");;            :depends-on ("SlotDef" "RDFboot"))
                    (:file "domain-range")  ;;     :depends-on ("RDFboot"))
                    (:file "rdfs-objects")  ;;     :depends-on ("RDFboot" "GxType"))
                    (:file "rdfs-kernel")  ;;      "RDFboot"))
                    (:file "gx-forward-ref")
                    (:file "rdfs-core")
                    (:file "gx-utils")
                    (:file "rdf-writer")
                  #+nil  (:file "rdf-walker")))
    
    (:module OWL :depends-on (RDFS)
      :serial t
      :components ((:file "nnf")
                    (:file "tunify")
                    (:file "subsume"         :depends-on ("nnf" "tunify"))
                    (:file "owl"             :depends-on ("subsume"))))
    
    (:module NTRIPLE :depends-on (OWL)
      :serial t
      :components ((:file "ntriple")
                    (:file "nt-parser"        :depends-on ("ntriple"))
                    (:file "nt-writer"        :depends-on ("nt-parser"))))
    
    (:module UTILITY :depends-on (NTRIPLE)
      :serial t
      :components ((:file "hyperspec")
                    (:file "utilities")
                    (:file "porcelain")
                    (:file "readtable")
;;                    (:file "unify")
                    (:file "nnf")
                    (:file "infix")))
    
    (:module ONTOLOGY :depends-on (UTILITY)
      :serial t
      :components ((:file  "dc")
                    (:file "composite")
                    (:file "rss")
                    (:file "earl")
                    (:file "change") 
                    (:file "skos")   
                    (:file "tables")  
                    (:file "calendar") 
                    (:file "sxml")
                    (:file "dot")
                    (:file "imports")
                    ))
    (:file "gx-user")
    
#+later    (:module SERVICE :deopends-on (ONTOLOGY)
      :serial t
      :components ((:file "session")
    ))))

;;    (:module RDFA)
;;    (:module REST)
;;    (:module DATA)
;;    ))

