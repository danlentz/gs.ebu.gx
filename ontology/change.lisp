;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-
;;;;;
;;;;; change.lisp
;;;;;
;;;;; An RDF model that can be used to represent changes to an RDF graph.
;;;;; change:Change represents a group of added or deleted triples (instances of rdf:Statement)
;;;;;
;;;;; Author:      Dan Lentz, Lentz Intergalactic Softworks
;;;;; Maintainer:  <danlentz@gmail.com>
;;;;;

(in-package :gx-user)


(defpackage :change (:use)
            (:export :Change :deleted :added :timeStamp :graph)
            (:documentation "http://www.topbraid.org/change"))


;;;
;;; CHANGE Ontology
;;;

(defIndividual change::Ontology (rdf:type owl:Ontology)
  (rdfs:label (@ "CHANGE 1.0.0 Schema" "en"))
  (dc:title (@ "RDF Model of Changes" "en"))
  (rdf:about "http://www.topbraid.org/change")
;;  (owl:versionInfo (^^ "1.1.0" xsd:string)
  (rdfs:comment "An RDF model that can be used to represent changes to
  an RDF graph. The base class is change:Change which represents a
  group of added or deleted triples (instances of rdf:Statement)"@en))


;;;
;;; CHANGE Classes (Nodes)
;;;

(defConcept change::Change (rdfs:subClassOf rdfs:Resource)
  (rdfs:label (@ "Change" "en"))
  (rdfs:comment (@ "A change to an RDF Graph, encapsulating lists of
  added and/or deleted rdf:Statements." "en")))

;;;
;;; CHANGE Properties (Edges)
;;;

(defProperty change::deleted (rdf:type rdf:Property)
  (rdfs:label (@ "Deleted" "en"))
  (rdfs:domain change::Change)
  (rdfs:range  rdf:Statement)
  (rdfs:comment (@ "Links a Change with an rdf:Statement that has been deleted" "en")))

(defProperty change::added (rdf:type rdf:Property)
  (rdfs:label (@ "Added" "en"))
  (rdfs:domain change::Change)
  (rdfs:range  rdf:Statement)
  (rdfs:comment (@ "Links a Change with an rdf:Statement that has been added" "en")))

(defProperty change::timeStamp (rdf:type rdf:Property)
  (rdfs:label (@ "Time Stamp" "en"))
  (rdfs:domain change::Change)
  (rdfs:range  xsd::dateTime)
  (rdfs:comment (@ "Time at which a Change occurs" "en")))

(defProperty change::graph (rdf:type rdf:Property)
  (rdfs:label (@ "Graph" "en"))
  (rdfs:domain rdf:Statement)
  (rdfs:range  rdfs:Resource)
  (rdfs:comment (@ "The URI of the (optional) graph that this statement has been modified in"
                   "en")))


;;;;;
;; Local Variables:
;; mode: outline-minor
;; indent-tabs: nil
;; outline-regexp: ";;[;]+"
;; page-delimiter: ""
;; End:
