;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-
;;;;;
;;;;; calendar.lisp
;;;;;
;;;;;   An ontology to represent temporal concepts
;;;;;
;;;;; Author:      Dan Lentz, Lentz Intergalactic Softworks
;;;;; Maintainer:  <danlentz@gmail.com>
;;;;;

(in-package :gx-user)

(defpackage :calendar (:use)
  (:documentation "http://www.topbraid.org/2007/01/calendar.owl")
  (:export :Event :startTime :endTime))
(set-uri-namedspace-from-pkg (find-package :calendar))


;;;
;;; CALENDAR Ontology
;;;

(defIndividual calendar::Ontology (rdf:type owl:Ontology)
  ;;  (owl:versionInfo (^^ "1.0.0" xsd:string))
  (rdfs:label (@ "CALENDAR 1.0.0 Schema" "en"))
  (dc::title  (@ "RDF representation of Temporal Concepts" "en"))
  (rdfs:comment (@ "An ontology that provides a semantic foundation
  for temporal events. The ontology defines start and end time
  properties. These can be used by calendars and other mashup tools to
  identify values that shall be displayed as start and end times of an
  event.  Users can import this ontology and make their own start and
  end time properties subproperties of these base properties"
                    "en")))

;;;
;;; CALENDAR Classes (Nodes)
;;;

(defConcept calendar::Event (rdf:type owl:Class)
  (rdfs:label   (@ "Event" "en"))
  (rdfs:comment (@ "Events are resources with a start and an end time" "en")))

;;;
;;; CALENDAR Properties (Edges)
;;;

(defProperty calendar::startTime (rdf:type owl:DatatypeProperty)
  (rdfs:label (@ "Start Time" "en"))
  (rdfs:domain calendar::Event)
  (rdfs:range  xsd::dateTime)) ;;; FIXME

(defProperty calendar::endTime (rdf:type owl:DatatypeProperty)
  (rdfs:label (@ "End Time" "en"))
  (rdfs:domain calendar::Event)
  (rdfs:range  xsd::dateTime)) ;;; FIXME


;;;;;
;; Local Variables:
;; mode: outline-minor
;; indent-tabs: nil
;; outline-regexp: ";;[;]+"
;; End:
