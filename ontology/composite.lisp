;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-
;;;;;
;;;;; composite.lisp
;;;;;
;;;;;   A simple ontology to help representing part-of (parent/child) relationships
;;;;;
;;;;; Author:      Dan Lentz, Lentz Intergalactic Softworks
;;;;; Maintainer:  <danlentz@gmail.com>
;;;;;

(in-package :gx-user)

(defpackage :composite (:use)
            (:export :parent :child :index)
            (:documentation "http://www.topbraid.org/2007/05/composite.owl"))


;;;
;;; COMPOSITE Ontology
;;;

(defIndividual composite::Ontology (rdf:type owl:Ontology)
;;  (owl:versionInfo (^^ "1.0.0" xsd:string))
  (rdfs:label (@ "COMPOSITE 1.0.0 Schema" "en"))
  (dct::title (@ "RDF Composite/Hierarchical Stucture" "en"))
  (rdfs:comment   (@ "A simple ontology to help representing part-of (parent/child) relationships."
                     "en")))

;;;
;;; COMPOSITE Properties
;;;

(defProperty composite::parent (rdf:type owl:ObjectProperty)
  (rdfs:label   (@ "Parent" "en"))
  (owl:inverseOf composite::child)
  (rdfs:comment (@ "The object is the parent of the subject" "en")))

(defProperty composite::child (rdf:type owl:ObjectProperty)
  (rdfs:label (@ "Child" "en"))
  (owl:inverseOf composite::parent)
  (rdfs:comment (@ "The object is the child of the subject" "en")))

(defProperty composite::index (rdf:type owl:DatatypeProperty)
  (rdfs:label (@ "Index" "en"))
  (rdfs:range xsd:int)
  (rdfs:comment (@ "The index of the subject among its siblings" "en")))

;;;;;
;; Local Variables:
;; mode: outline-minor
;; indent-tabs: nil
;; outline-regexp: ";;[;]+"
;; End:
