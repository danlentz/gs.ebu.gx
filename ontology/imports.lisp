;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-
;;;;;
;;;;; imports.lisp
;;;;;
;;;;; Author:      Dan Lentz, Lentz Intergalactic Softworks
;;;;; Maintainer:  <danlentz@gmail.com>
;;;;;

(in-package :gx-user)

(defpackage :smtp (:use)
  (:documentation "mailto://"))

(defpackage :wn16 (:use)
  (:documentation "http://xmlns.com/wordnet/1.6/"))
(set-uri-namedspace-from-pkg :wn16)

(read-rdf-file #'addRdfXml #p"gx:ontology;doap.rdf")

(read-rdf-file #'addRdfXml #p"gx:ontology;time.owl")

(defpackage :void (:use)
  (:documentation "http://vocab.deri.ie/void"))
(set-uri-namedspace-from-pkg :void)

(defpackage :scovo (:use)
  (:documentation "http://purl.org/NET/scovo"))
(set-uri-namedspace-from-pkg :scovo)

(read-rdf-file #'addRdfXml #p"gx:ontology;void.rdf")

(defpackage :content (:use)
  (:documentation "http://www.w3.org/2007/content")
(set-uri-namedspace-from-pkg :content)

(read-rdf-file #'addRdfXml #p"gx:ontology;content.rdf")



(defpackage :skos (:use)
  (:documentation  "http://www.w3.org/2008/05/skos"))))

(defpackage :trix (:use)
  (:documentation "http://www.w3.org/2004/03/trix/rdfg-1/"))

(defpackage :sioc (:use)
  (:documentation "http://rdfs.org/sioc"))
(set-uri-namedspace-from-pkg :sioc)

(defpackage :sioc-types (:use)
  (:documentation "http://rdfs.org/sioc/types"))
(set-uri-namedspace-from-pkg :sioc-types)

(defpackage :sioc-services (:use)
  (:documentation "http://rdfs.org/sioc/services"))
(set-uri-namedspace-from-pkg :sioc-services)

(defpackage :sioc-access (:use)
  (:documentation "http://rdfs.org/sioc/access"))
(set-uri-namedspace-from-pkg :sioc-access)

    
(read-rdf-file #'addRdfXml #p"gx:ontology;sioc.owl")
(read-rdf-file #'addRdfXml #p"gx:ontology;sioc-types.owl")
(read-rdf-file #'addRdfXml #p"gx:ontology;sioc-services.owl")
(read-rdf-file #'addRdfXml #p"gx:ontology;sioc-access.owl")


(defpackage :http (:use)
  (:documentation "http://www.w3.org/2006/http"))
(set-uri-namedspace-from-pkg :http)


(defpackage :vom (:use)
  (:documentation "http://www.ifi.unizh.ch/ddis/evoont/2008/11/vom#"))
(set-uri-namedspace-from-pkg :vom)

(defpackage :som (:use)
  (:documentation "http://www.ifi.unizh.ch/ddis/evoont/2008/11/som#"))
(set-uri-namedspace-from-pkg :som)
;;(read-rdf-file #'addRdfXml #p"gx:ontology;http.rdf")
(read-rdf-file #'addRdfXml #p"gx:ontology;som.owl")
;;(read-rdf-file #'addRdfXml #p"gx:data;doap;redland-doap.rdf")

;;;;;
;; Local Variables:
;; mode: outline-minor
;; indent-tabs: nil
;; outline-regexp: ";;[;]+"
;; End:
