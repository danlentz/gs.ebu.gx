;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-
;;;;;
;;;;; sxml.lisp
;;;;;
;;;;; An ontology describing XML documents (elements and their hierarchy).
;;;;; XML Documents have a root element. Attributes are represented as values of datatype properties
;;;;; that have a value for sxml:attribute. Future versions may also support object properties to
;;;;; resolve IDREF attributes. Elements are represented as instances of classes that have an
;;;;; sxml:element annotation.
;;;;;
;;;;; Author:      Dan Lentz, Lentz Intergalactic Softworks
;;;;; Maintainer:  <danlentz@gmail.com>
;;;;;

(in-package :gx-user)

(defpackage :sxml (:use)
  (:export :Node :Document :TextNode :CDATA :Element :Comment
    :root :text :attribute :element :prefix)
  (:documentation "http://topbraid.org/sxml"))
(set-uri-namedspace-from-pkg (find-package :sxml))
  
;;;
;;; SXML Ontology
;;;


(defIndividual sxml::Ontology (rdf:type owl:Ontology)
  (rdf:about "http://topbraid.org/sxml")
  (owl:imports (uri "http://www.topbraid.org/2007/05/composite.owl"))
;;  (owl:versionInfo (^^ "1.2.0" xsd:string))
  (rdfs:label (@ "SXML 1.2.0 Schema" "en"))
  (dct::title (@ "SXML RDF representation of XML" "en"))
  (rdfs:comment   (@ "An ontology describing XML documents (elements and their hierarchy).
XML Documents have a root element. Attributes are represented as values of datatype properties that have a value for sxml:attribute. Future versions may also support object properties to resolve IDREF attributes. Elements are represented as instances of classes that have an sxml:element annotation"
      "en")))

;;;
;;; SXML Classes (Nodes)
;;;

(defConcept sxml::Node (rdfs:subClassOf owl:Thing)
  (rdfs:label (@ "Node" "en")))

(defConcept sxml::Document (rdfs:subClassOf sxml::Node)
  (rdfs:label (@ "Document" "en")))

(defConcept sxml::TextNode (rdfs:subClassOf sxml::Node)
  (rdfs:label (@ "Text Node" "en")))

(defConcept sxml::CDATA (rdfs:subClassOf sxml::Node)
  (rdfs:label (@ "CDATA" "en")))

(defConcept sxml::Element (rdfs:subClassOf sxml::Node)
  (rdfs:label (@ "Element" "en")))

(defConcept sxml::Comment (rdfs:subClassOf sxml::Node)
  (rdfs:label (@ "Comment" "en")))

;;;
;;; SXML Properties (Edges)
;;;

(defProperty sxml::root (rdf:type owl:ObjectProperty)
  (rdfs:domain sxml:Document)
  (rdfs:subPropertyOf composite:child)
  (rdfs:comment   (@ "The root element of an XML document" "en")))

(defProperty sxml::text (rdf:type owl:DatatypeProperty)
  (rdfs:label (@ "Text" "en"))
  (rdfs:domain sxml:TextNode)
  (rdfs:range  xsd:string)
  (rdfs:comment   (@ "The body of an XML TextNode" "en")))

(defProperty sxml::attribute (rdf:type owl:AnnotationProperty)
  (rdfs:label (@ "Attribute" "en"))
  (rdfs:domain rdf:Property)
  (rdfs:comment (@ "The XML tag used to render an RDF property into an
  XML attribute. This must be a valid XML attribute name" "en")))

(defProperty sxml::element (rdf:type owl:AnnotationProperty)
  (rdfs:label (@ "Element" "en"))
  (rdfs:domain rdfs:Class)
  (rdfs:comment (@ "The XML tag used to render an element. The values
  of this property must be valid XML tag strings or URI resources that
  can be abbreviated by an existing namespace prefix. For example, a
  tag <process> would be mapped into a class with
  sxml:element=\"process\"^^xsd:string. A tag <my:process> would be
  mapped into a class with sxml:element=my:process (which is often
  identical to the URI of the class itself)." "en")))

(defProperty sxml::prefix (rdf:type owl:DatatypeProperty)
  (rdfs:label (@ "Prefix" "en"))
  (rdfs:comment (@ "Stores the name of the prefixes that will be
  written back to the XML file when saved. This is mainly for
  round-tripping XML files that have xmlns attributes in the root
  element" "en")))


;;;;;
;; Local Variables:
;; mode: outline-minor
;; indent-tabs: nil
;; outline-regexp: ";;[;]+"
;; End:
