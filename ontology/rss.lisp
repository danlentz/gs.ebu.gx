;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-
;;;;;
;;;;; rss.lisp
;;;;;
;;;;;   Schema declaration for Rich Site Summary (RSS) 1.0
;;;;;
;;;;; Author:      Dan Lentz, Lentz Intergalactic Softworks
;;;;; Maintainer:  <danlentz@gmail.com>
;;;;;

(in-package :gx-user)

(defpackage :rss (:use)
            (:export :channel :image :item :textinput
                     :items :title :link :url :description :name)
            (:documentation "http://purl.org/rss/1.0/"))

(defIndividual rss::Ontology (rdf:type owl:Ontology)
  (rdf:about "http://purl.org/rss/1.0/")
  (rdfs:label   (@ "RSS 1.0 Schema" "en"))
  (dc::title    (@ "Rich Site Summary Language" "en"))
  (rdfs:comment (@ "RDF Schema declaration for Rich Site Summary (RSS) 1.0 http://purl.org/rss/1.0/"
      "en")))  

;;;
;;; RSS Classes (Nodes)
;;;

(defConcept rss::channel (rdf:type rdfs:Class)
  (rdfs:label (@ "Channel" "en"))
  (rdfs:comment (@ "An RSS information channel" "en"))
  (rdfs:isDefinedBy "http://purl.org/rss/1.0/"))
(defConcept rss::Channel (rdfs:subClassOf rss::channel))

(defConcept rss::image (rdf:type rdfs:Class)
  (rdfs:label (@ "Image" "en"))
  (rdfs:comment (@ "An RSS image" "en"))
  (rdfs:isDefinedBy "http://purl.org/rss/1.0/"))
(defConcept rss::Image (rdfs:subClassOf rss::image))

(defConcept rss::item (rdf:type rdfs:Class)
  (rdfs:label (@ "Item" "en"))
  (rdfs:comment (@ "An RSS item" "en"))
  (rdfs:isDefinedBy "http://purl.org/rss/1.0/"))
(defConcept rss::Item (rdfs:subClassOf rss::item))

(defConcept rss::textinput (rdf:type rdfs:Class)
  (rdfs:label (@ "Text Input" "en"))
  (rdfs:comment (@ "An RSS text input" "en"))
  (rdfs:isDefinedBy "http://purl.org/rss/1.0/"))
(defConcept rss::TextInput (rdfs:subClassOf rss::textinput))

;;;
;;; RSS Properties (Edges)
;;;

(defProperty rss::items (rdf:type rdf:Property)
  (rdfs:label (@ "Items" "en"))
  (rdfs:comment (@ "Points to a list of rss:item elements that are members of the subject channel"
                   "en"))
  (rdfs:isDefinedBy "http://purl.org/rss/1.0/"))

(defProperty rss::title (rdfs:subPropertyOf dc::title)
  (rdfs:label (@ "Title" "en"))
  (rdfs:range rdfs:Literal)
  (rdfs:comment   (@ "A descriptive title for the channel" "en"))
  (rdfs:isDefinedBy "http://purl.org/rss/1.0/"))

(defProperty rss::link (rdfs:subPropertyOf dc::identifier)
  (rdfs:label (@ "Link" "en"))
  (rdfs:range xsd:anyURI)
  (rdfs:comment   (@ "The URL to which an HTML rendering of the subject will link" "en"))
  (rdfs:isDefinedBy "http://purl.org/rss/1.0/"))

(defProperty rss::url (rdfs:subPropertyOf dc::identifier)
  (rdfs:label (@ "URL" "en"))
  (rdfs:range xsd:anyURI)
  (rdfs:comment  (@ "The URL of the image to used in the 'src' attribute of the channel's image tag when rendered as HTML" "en"))
  (rdfs:isDefinedBy "http://purl.org/rss/1.0/"))

(defProperty rss::description (rdfs:subPropertyOf dc::description)
  (rdfs:label (@ "Description" "en"))
  (rdfs:comment   (@ "A short text description of the subject" "en"))
  (rdfs:isDefinedBy "http://purl.org/rss/1.0/"))

(defProperty rss::name (rdf:type rdf:Property)
  (rdfs:label (@ "Name" "en"))
  (rdfs:comment   (@ "The text input field's (variable) name" "en"))
  (rdfs:isDefinedBy "http://purl.org/rss/1.0/"))

;;;;;
;; Local Variables:
;; mode: outline-minor
;; indent-tabs: nil
;; outline-regexp: ";;[;]+"
;; End:
