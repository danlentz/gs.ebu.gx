;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-
;;;;;
;;;;; tables.lisp
;;;;;
;;;;; A simple ontology that can be used to annotate other ontologies so that they can be mapped
;;;;; to tabular formats such as spreadsheets. 
;;;;;
;;;;; Author:      Dan Lentz, Lentz Intergalactic Softworks
;;;;; Maintainer:  <danlentz@gmail.com>
;;;;;

(in-package :gx-user)

;;;
;;; TABLES Ontology
;;;

(defpackage :tables (:use)
            (:export :rowIndex :columnIndex :sheetIndex)
            (:documentation  "http://topbraid.org/tables"))
(set-uri-namedspace-from-pkg (find-package :tables))

(defIndividual tables::Ontology (rdf:type owl:Ontology)
  (rdf:about  "http://topbraid.org/tables")
  (rdfs:label (@ "TABLES 1.0.0 Schema" "en"))
  (dc::title (@ "TABLES RDF representation of tabular content" "en"))
;;  (owl:versionInfo (^^ "1.0.0" xsd:string))
  (rdfs:comment (@ "A simple ontology that can be used to annotate
  other ontologies so that they can be mapped to tabular formats such
  as spreadsheets. This ontology is used, for example, by the Excel
  back-end of TopBraid" "en")))

(defProperty tables::rowIndex (rdf:type owl:AnnotationProperty)
  (rdfs:label (@ "Row Index" "en"))
  (rdfs:range xsd:int)
  (rdfs:comment (@ "This property can be used to annotate instances to
  instruct the engine that this instance shall be written to a row at
  a given index" "en")))

(defProperty tables::columnIndex (rdf:type owl:AnnotationProperty)
  (rdfs:label (@ "Column Index" "en"))
  (rdfs:range xsd:int)
  (rdfs:comment (@ "This property can be used to annotate instances to
  instruct the engine that this instance shall be written to a column
  at a given index" "en")))

(defProperty tables::sheetIndex (rdf:type owl:AnnotationProperty)
  (rdfs:label (@ "Sheet Index" "en"))
  (rdfs:range xsd:int)
  (rdfs:comment (@ "This property can be used to annotate instances to
  instruct the engine that this instance shall be written to a sheet
  at a given index" "en")))


;;;;;
;; Local Variables:
;; mode: outline-minor
;; indent-tabs: nil
;; outline-regexp: ";;[;]+"
;; End:
