;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; Packages in RDFS System
;;;
;;; This module defines basic symbols in xml, xsd, rdf, rdfs, and owl package.
;;; Those symbols are exported so as to be QNames.

(in-package :cl-user)

(require :cg)

(defpackage :_     (:use))
(defpackage :x     (:use :cl))
(defpackage :xmlns (:use))
(defpackage :xml   (:use) (:export lang))

(defpackage :xsd   (:use) (:documentation "http://www.w3.org/2001/XMLSchema#")
            (:export string boolean decimal float double dataTime
                     time date base64Binary unsignedByte Name NCName
                     gYearMonth gYear gMonthDay gDay gMonth hexBinary
                     anyURI normallizedString token language NMTOKEN
                     integer nonPositiveInteger negativeInteger long int 
                     nonNegativeInteger unsignedLong unsignedInt unsignedShort
                     duration-hour duration-minute duration-second short byte
                     positiveInteger simpleType anySimpleType true false
                     duration duration-year duration-month duration-day))

(defpackage :rdf   (:use) (:documentation "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
            (:export _1 _2 _3 _4 _5 _6 _7 _8 _9 about XMLDatatype inLang 
                     resource ID parseType datatype nodeID nil first rest 
                     Property List Statement subject predicate object 
                     Bag Seq Alt value li XMLDecl RDF Description type
                     XMLLiteral))

(defpackage :rdfs  (:use) (:documentation "http://www.w3.org/2000/01/rdf-schema#")
            (:export Resource Class subClassOf subPropertyOf seeAlso domain range
                     isDefinedBy range domain Literal Container label comment member
                     ContainerMembershipProperty Datatype))

(defpackage :owl   (:use) (:documentation "http://www.w3.org/2002/07/owl#")
            (:export Class Thing Nothing Restriction onProperty allValuesFrom
                     someValuesFrom hasValue minCardinality maxCardinality cardinality
                     allValuesFromRestriction someValuesFromRestriction hasValueRestriction 
                     cardinalityRestriction Ontology distinctMembers equivalentClass
                     oneOf differentFrom sameAs AllDifferent describe-slot-constraint 
                     TransitiveProperty ObjectProperty DatatypeProperty FunctionalProperty 
                     InverseFunctionalProperty SymmetricProperty inverseOf backwardCompatibleWith
                     intersectionOf unionOf disjointWith complementOf equivalentProperty
                     DataRange DeprecatedProperty DeprecatedClass incompatibleWith 
                     priorVersion versionInfo imports OntologyProperty AnnotationProperty))

(defpackage :gx-test (:use :cl))

