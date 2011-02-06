;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-
;;;;;
;;;;; dc.lisp
;;;;;
;;;;;   dc: Dublin Core Element Set
;;;;;
;;;;; Author:      Dan Lentz, Lentz Intergalactic Softworks
;;;;; Maintainer:  <danlentz@gmail.com>
;;;;;

(in-package :gx-user)

;;;
;;; DC Namespace
;;;

(defpackage :skos    (:use) (:documentation "http://www.w3.org/2004/02/skos/core#"))
(defpackage :dctype  (:use) (:documentation "http://purl.org/dc/dcmitype/"))
(defpackage :dcns    (:use) (:documentation "http://purl.org/dc/"))
(defpackage :dcabout (:use) (:documentation "http://purl.org/dc/about/"))
(defpackage :dcam    (:use) (:documentation "http://purl.org/dc/dcam/"))
(defpackage :dchist  (:use) (:documentation "http://purl.org/dc/usage/history/"))
(defpackage :dct     (:use) (:documentation "http://purl.org/dc/terms/"))            

(defpackage :dc      (:use) (:documentation "http://purl.org/dc/elements/1.1/")
  (:export #:|1.1| #:contributor #:coverage #:creator #:date #:description
    #:language #:publisher #:format #:identifier
    #:relation #:rights #:source #:subject #:title #:type))

(set-uri-namedspace-from-pkg (find-package :skos))
(set-uri-namedspace-from-pkg (find-package :dctype))
(set-uri-namedspace-from-pkg (find-package :dcns))
(set-uri-namedspace-from-pkg (find-package :dc))
(set-uri-namedspace-from-pkg (find-package :dcam))
(set-uri-namedspace-from-pkg (find-package :dct))
(set-uri-namedspace-from-pkg (find-package :dchist))
(set-uri-namedspace-from-pkg (find-package :dcabout))

;;;
;;; SKOS Ontology
;;;

(addForm '(rdf:Description dc:|1.1| (rdf:about "http://purl.org/dc/elements/1.1/")
           (dc::title (:en-US "Dublin Core Metadata Element Set, Version 1.1"))
           (dc::publisher dcabout::DCMI) (dc::modified "2010-10-11")))


(defProperty dc:title (rdf:type rdf:Property)
  (rdf:about "http://purl.org/dc/elements/1.1/title")
  (rdfs:label (:en-US "Title"))
  (rdfs:isDefinedBy  "http://purl.org/dc/elements/1.1/")
  (rdfs:comment (:en-US "A name given to the resource."))
  (skos::note (:en-US "A second property with the same name as this property has been declared in the dcterms: namespace (http://purl.org/dc/terms/).  See the Introduction to the document \"DCMI Metadata Terms\" (http://dublincore.org/documents/dcmi-terms/) for an explanation."))
  (dct::hasVersion dchist::title-006) (dc::modified "2008-01-14")
  (dc::issued "1999-07-02"))


(defProperty dc:creator (rdf:type rdf:Property)
  (rdf:about "http://purl.org/dc/elements/1.1/creator" )
  (rdfs:label (:en-US "Creator"))
  (rdfs:isDefinedBy "http://purl.org/dc/elements/1.1/" )
  (rdfs:comment  (:en-US "An entity primarily responsible for making the resource."))
  (skos::note     (:en-US   "A second property with the same name as this property has been declared in the dcterms: namespace (http://purl.org/dc/terms/).  See the Introduction to the document \"DCMI Metadata Terms\" (http://dublincore.org/documents/dcmi-terms/) for an explanation."))
  (dct::hasVersion dchist::creator-006) (dc::modified "2008-01-14") (dct::issued "1999-07-02")
  (dc::description (:en-US  "Examples of a Creator include a person, an organization, or a service. Typically, the name of a Creator should be used to indicate the entity.")))

(defProperty dc:subject (rdf:type rdf:Property)
  (rdf:about "http://purl.org/dc/elements/1.1/subject")
  (rdfs:label (:en-US "Subject"))
  (rdfs:isDefinedBy "http://purl.org/dc/elements/1.1/")
  (rdfs:comment (:en-US "The topic of the resource."))
  (skos::note (:en-US "A second property with the same name as this property has been declared in the dcterms: namespace (http://purl.org/dc/terms/).  See the Introduction to the document \"DCMI Metadata Terms\" (http://dublincore.org/documents/dcmi-terms/) for an explanation."))
  (dct::hasVersion dchist::subject-006) (dct::modified "2008-01-14")  (dct::issued "1999-07-02")
  (dct::description (:en-US  "Typically, the subject will be represented using keywords, key phrases, or classification codes. Recommended best practice is to use a controlled vocabulary. To describe the spatial or temporal topic of the resource, use the Coverage element.")))
  
(defProperty dc:format  (rdf:type rdf:Property)
  (rdf:about "http://purl.org/dc/elements/1.1/format")
  (rdfs:label   (:en-US "Format"))
  (rdfs:isDefinedBy "http://purl.org/dc/elements/1.1/")
  (rdfs:comment (:en-US  "The file format, physical medium, or dimensions of the resource."))
  (skos::note    (:en-US  "A second property with the same name as this property has been declared in the dcterms: namespace (http://purl.org/dc/terms/).  See the Introduction to the document \"DCMI Metadata Terms\" (http://dublincore.org/documents/dcmi-terms/) for an explanation."))
  (dct::hasVersion dchist::format-007) (dct::modified "2008-01-14")  (dct::issued "1999-07-02")
  (dct::description (:en-US "Examples of dimensions include size and duration. Recommended best practice is to use a controlled vocabulary such as the list of Internet Media Types [MIME].")))

(defProperty dc:contributor  (rdf:type rdf:Property)
  (rdf:about "http://purl.org/dc/elements/1.1/contributor")
  (rdfs:label (:en-US "Contributor"))
  (rdfs:isDefinedBy "http://purl.org/dc/elements/1.1/")
  (rdfs:comment (:en-US  "An entity responsible for making contributions to the resource."))
  (skos::note (:en-US  "A second property with the same name as this property has been declared in the dcterms: namespace (http://purl.org/dc/terms/).  See the Introduction to the document \"DCMI Metadata Terms\" (http://dublincore.org/documents/dcmi-terms/) for an explanation."))
  (dct::hasVersion dchist::contributor-006) (dct::modified "2008-01-14") (dct::issued "1999-07-02")
  (dct::description (:en-US "Examples of a Contributor include a person, an organization, or a service. Typically, the name of a Contributor should be used to indicate the entity.")))

 (defProperty dc:rights  (rdf:type rdf:Property)
  (rdf:about "http://purl.org/dc/elements/1.1/rights")
  (rdfs:label    (:en-US "Rights"))
  (rdfs:isDefinedBy "http://purl.org/dc/elements/1.1/")
  (rdfs:comment  (:en-US "Information about rights held in and over the resource."))
  (skos::note     (:en-US "A second property with the same name as this property has been declared in the dcterms: namespace (http://purl.org/dc/terms/).  See the Introduction to the document \"DCMI Metadata Terms\" (http://dublincore.org/documents/dcmi-terms/) for an explanation."))
  (dct::hasVersion dchist::rights-006) (dct::modified "2008-01-14") (dct::issued "1999-07-02")
  (dct::description  (:en-US "Typically, rights information includes a statement about various property rights associated with the resource, including intellectual property rights.")))

 (defProperty dc:date  (rdf:type rdf:Property)
  (rdf:about "http://purl.org/dc/elements/1.1/date")
  (rdfs:label (:en-US "Date"))
  (rdfs:isDefinedBy "http://purl.org/dc/elements/1.1/")
  (rdfs:comment (:en-US "A point or period of time associated with an event in the lifecycle of the resource."))
  (skos::note  (:en-US   "A second property with the same name as this property has been declared in the dcterms: namespace (http://purl.org/dc/terms/).  See the Introduction to the document \"DCMI Metadata Terms\" (http://dublincore.org/documents/dcmi-terms/) for an explanation."))
  (dct::hasVersion dchist::date-006) (dct::modified "2008-01-14") (dct::issued "1999-07-02")
  (dct::description (:en-US "Date may be used to express temporal information at any level of granularity.  Recommended best practice is to use an encoding scheme, such as the W3CDTF profile of ISO 8601 [W3CDTF].")))
  
 (defProperty dc:language  (rdf:type rdf:Property)
  (rdf:about "http://purl.org/dc/elements/1.1/language")
  (rdfs:label (:en-US "Language"))
  (rdfs:isDefinedBy "http://purl.org/dc/elements/1.1/")
  (rdfs:comment (:en-US "A language of the resource."))
  (skos::note  (:en-US   "A second property with the same name as this property has been declared in the dcterms: namespace (http://purl.org/dc/terms/).  See the Introduction to the document \"DCMI Metadata Terms\" (http://dublincore.org/documents/dcmi-terms/) for an explanation."))
  (dct::hasVersion dchist::language-007) (dct::modified "2008-01-14") (dct::issued "1999-07-02")
  (dct::description (:en-US "Recommended best practice is to use a controlled vocabulary such as RFC 4646 [RFC4646]."))
  (rdfs:seeAlso "http://www.ietf.org/rfc/rfc4646.txt"))

  
 (defProperty dc:publisher  (rdf:type rdf:Property)
  (rdf:about "http://purl.org/dc/elements/1.1/publisher")
  (rdfs:label   (:en-US "Publisher"))
  (rdfs:isDefinedBy "http://purl.org/dc/elements/1.1/")
  (rdfs:comment (:en-US "An entity responsible for making the resource available."))
  (skos::note    (:en-US "A second property with the same name as this property has been declared in the dcterms: namespace (http://purl.org/dc/terms/).  See the Introduction to the document \"DCMI Metadata Terms\" (http://dublincore.org/documents/dcmi-terms/) for an explanation."))
  (dct::hasVersion dchist::publisher-006) (dct::modified "2008-01-14") (dct::issued "1999-07-02")
  (dct::description (:en-US "Examples of a Publisher include a person, an organization, or a service. Typically, the name of a Publisher should be used to indicate the entity.")))

 (defProperty dc:type   (rdf:type rdf:Property)
  (rdf:about "http://purl.org/dc/elements/1.1/type")
  (rdfs:label (:en-US "Type"))
  (rdfs:isDefinedBy "http://purl.org/dc/elements/1.1/")
  (rdfs:comment (:en-US "The nature or genre of the resource."))
  (skos::note    (:en-US "A second property with the same name as this property has been declared in the dcterms: namespace (http://purl.org/dc/terms/).  See the Introduction to the document \"DCMI Metadata Terms\" (http://dublincore.org/documents/dcmi-terms/) for an explanation."))
  (dct::hasVersion dchist::type-006) (dct::modified "2008-01-14") (dct::issued "1999-07-02")
  (dc:description (:en-US  "Recommended best practice is to use a controlled vocabulary such as the DCMI Type Vocabulary [DCMITYPE]. To describe the file format, physical medium, or dimensions of the resource, use the Format element.")))

 (defProperty dc:source  (rdf:type rdf:Property)
  (rdf:about "http://purl.org/dc/elements/1.1/source")
  (rdfs:label (:en-US "Source"))
  (rdfs:isDefinedBy "http://purl.org/dc/elements/1.1/")
  (rdfs:comment (:en-US "A related resource from which the described resource is derived."))
  (skos::note    (:en-US "A second property with the same name as this property has been declared in the dcterms: namespace (http://purl.org/dc/terms/).  See the Introduction to the document \"DCMI Metadata Terms\" (http://dublincore.org/documents/dcmi-terms/) for an explanation."))
  (dct::hasVersion dchist::source-006) (dct::modified "2008-01-14")  (dct::issued "1999-07-02")
  (dct::description  (:en-US   "The described resource may be derived from the related resource in whole or in part. Recommended best practice is to identify the related resource by means of a string conforming to a formal identification system.")))

 (defProperty dc:description   (rdf:type rdf:Property)
  (rdf:about "http://purl.org/dc/elements/1.1/description")
  (rdfs:label (:en-US "Description"))
  (rdfs:isDefinedBy "http://purl.org/dc/elements/1.1/")
  (rdfs:comment (:en-US "An account of the resource."))
  (skos::note    (:en-US "A second property with the same name as this property has been declared in the dcterms: namespace (http://purl.org/dc/terms/).  See the Introduction to the document \"DCMI Metadata Terms\" (http://dublincore.org/documents/dcmi-terms/) for an explanation."))
  (dct::hasVersion dchist::description-006) (dct::modified "2008-01-14")  (dct::issued "1999-07-02")
  (dct::description   (:en-US "Description may include but is not limited to: an abstract, a table of contents, a graphical representation, or a free-text account of the resource.")))

 (defProperty dc:coverage  (rdf:type rdf:Property)
  (rdf:about "http://purl.org/dc/elements/1.1/coverage")
  (rdfs:label     (:en-US "Coverage"))
  (rdfs:isDefinedBy "http://purl.org/dc/elements/1.1/")
  (rdfs:comment   (:en-US  "The spatial or temporal topic of the resource, the spatial applicability of the resource, or the jurisdiction under which the resource is relevant."))
  (skos::note      (:en-US  "A second property with the same name as this property has been declared in the dcterms: namespace (http://purl.org/dc/terms/).  See the Introduction to the document \"DCMI Metadata Terms\" (http://dublincore.org/documents/dcmi-terms/) for an explanation."))
  (dct::hasVersion dchist::coverage-006) (dct::modified "2008-01-14")  (dct::issued "1999-07-02")
  (dct::description (:en-US "Spatial topic and spatial applicability may be a named place or a location specified by its geographic coordinates. Temporal topic may be a named period, date, or date range. A jurisdiction may be a named administrative entity or a geographic place to which the resource applies. Recommended best practice is to use a controlled vocabulary such as the Thesaurus of Geographic Names [TGN]. Where appropriate, named places or time periods can be used in preference to numeric identifiers such as sets of coordinates or date ranges.")))

 (defProperty dc:identifier  (rdf:type rdf:Property)
  (rdf:about "http://purl.org/dc/elements/1.1/identifier")
  (rdfs:label   (:en-US "Identifier"))
  (rdfs:isDefinedBy "http://purl.org/dc/elements/1.1/")
  (rdfs:comment (:en-US   "An unambiguous reference to the resource within a given context."))
  (skos::note    (:en-US  "A second property with the same name as this property has been declared in the dcterms: namespace (http://purl.org/dc/terms/).  See the Introduction to the document \"DCMI Metadata Terms\" (http://dublincore.org/documents/dcmi-terms/) for an explanation."))
  (dct::hasVersion dchist::identifier-006) (dct::modified "2008-01-14")  (dct::issued "1999-07-02")
  (dct::description (:en-US "Recommended best practice is to identify the resource by means of a string conforming to a formal identification system.")))

(defProperty dc:relation  (rdf:type rdf:Property)
  (rdf:about "http://purl.org/dc/elements/1.1/relation")
  (rdfs:label   (:en-US "Relation"))
  (rdfs:isDefinedBy "http://purl.org/dc/elements/1.1/")
  (rdfs:comment (:en-US "A related resource."))
  (skos::note    (:en-US "A second property with the same name as this property has been declared in the dcterms: namespace (http://purl.org/dc/terms/).  See the Introduction to the document \"DCMI Metadata Terms\" (http://dublincore.org/documents/dcmi-terms/) for an explanation."))
  (dct::hasVersion dchist::relation-006) (dct::modified "2008-01-14")  (dct::issued "1999-07-02")
  (dct::description (:en-US  "Recommended best practice is to identify the related resource by means of a string conforming to a formal identification system."))) 
