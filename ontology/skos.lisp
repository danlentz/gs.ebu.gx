;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-
;;;;;
;;;;; skos.lisp
;;;;;
;;;;;   SKOS: Simple Knowledge Organization System
;;;;;
;;;;; Author:      Dan Lentz, Lentz Intergalactic Softworks
;;;;; Maintainer:  <danlentz@gmail.com>
;;;;;

(in-package :gx-user)

;;;
;;; SKOS Namespace
;;;

(defpackage :skos (:use)
  (:documentation "http://www.w3.org/2004/02/skos/core#")
  (:export
    
    ;; Classes
    #:Ontology #:Concept #:ConceptScheme #:Collection #:OrderedCollection

    ;; Properties
    #:narrower  #:broader  #:narrowMatch  #:broadMatch  #:closeMatch  #:exactMatch
    #:related   #:relatedMatch  #:semanticRelation  #:mappingRelation  #:broaderTransitive
    #:narrowerTransitive  #:memberList  #:hiddenLabel  #:prefLabel  #:altLabel
    #:definition  #:note  #:changeNote  #:historyNote  #:editorialNote  #:scopeNote
    #:example  #:inScheme  #:hasTopConcept  #:notation  #:member))

(set-uri-namedspace-from-pkg (find-package :skos))

;;;
;;; SKOS Ontology
;;;

(defIndividual skos::Ontology  (rdf:type owl:Ontology)
  (dc::title       (@ "SKOS Vocabulary" "en"))
  (rdfs:label      (@ "SKOS Vocabulary" "en"))
  (owl:versionInfo "1.0.0"^^xsd:string)
  (rdfs:seeAlso    (uri "http://www.w3.org/TR/skos-reference"))
  
  (dc::creator "Sean Bechhofer")   (dc::creator "Alistair Miles") 
  (dc::contributor "Nikki Rogers") (dc::contributor "Dave Beckett")

  (dc::description (@ "An RDF vocabulary for describing the basic
  structure and content of concept schemes such as thesauri,
  classification schemes, subject heading lists, taxonomies,
  'folksonomies', other types of controlled vocabulary, and also
  concept schemes embedded in glossaries and terminologies" "en")))

;;;
;;; SKOS Classes (nodes)
;;;

(defConcept skos::Concept (rdf:type owl:Class)
  (rdfs:label (@ "Concept" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (skos::definition (@ "An idea or notion; a unit of thought." "en")))

(defConcept skos::ConceptScheme (rdf:type owl:Class)
  (owl:disjointWith skos::Concept)
  (rdfs:label (@ "Concept Scheme" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (skos:definition (@ "A set of concepts, optionally including
  statements about semantic relationships between those
  concepts" "en"))
  (skos::example (@ "Thesauri, classification schemes, subject heading
  lists, taxonomies, 'folksonomies', and other types of controlled
  vocabulary are all examples of concept schemes. Concept schemes are
  also embedded in glossaries and terminologies" "en"))
  (skos::scopeNote (@ "A concept scheme may be defined to include
  concepts from different sources" "en")))

(defConcept skos::Collection  (rdf:type owl:Class)
    (rdfs:label (@ "Collection" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (owl:disjointWith skos::ConceptScheme)
  (owl:disjointWith skos::Concept)
  (skos::definition (@ "A meaningful collection of concepts" "en"))
  (skos::scopeNote (@ "Labelled collections can be used where you would
  like a set of concepts to be displayed under a 'node label' in the
  hierarchy" "en")))

(defConcept skos::OrderedCollection
    (rdfs:label (@ "Ordered Collection" "en"))
  (rdf:type owl:Class)
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:subClassOf  skos::Collection)
  (skos::definition (@ "An ordered collection of concepts, where both
  the grouping and the ordering are meaningful" "en"))
  (skos::scopeNote (@ "Ordered collections can be used where you would
  like a set of concepts to be displayed in a specific order, and
  optionally under a 'node label'" "en")))
   
;;;
;;; SKOS Properties (edges)
;;;

(defProperty skos::narrower (rdf:type owl:ObjectProperty)
  (rdfs:label (@ "has narrower" "en"))
  (owl:inverseOf skos::broader) (rdfs:subPropertyOf skos::narrowerTransitive)
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:comment (@ "Narrower concepts are typically rendered as
  children in a concept hierarchy (tree)" "en"))
  (skos::definition (@ "Relates a concept to a concept that is more specific in meaning" "en"))
  (skos::scopeNote (@ "By convention, skos:narrower is only used to
  assert an immediate (i.e. direct) hierarchical link between two
  conceptual resources" "en")))

(defProperty skos::broader (rdfs:label (@ "has broader" "en"))
  (rdf:type owl:ObjectProperty)
  (owl:inverseOf skos::narrower) (rdfs:subPropertyOf skos::broaderTransitive)
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:comment (@ "Broader concepts are typically rendered as parents
  in a concept hierarchy (tree)" "en"))
  (skos::definition (@ "Relates a concept to a concept that is more general in meaning" "en"))
  (skos::scopeNote (@ "By convention, skos:broader is only used to
  assert an immediate (i.e. direct) hierarchical link between two
  conceptual resources" "en")))

(defProperty skos::narrowMatch (rdf:type owl:ObjectProperty)
  (rdfs:label (@ "has narrower match" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (owl:inverseOf skos::broadMatch)
  (rdfs:subPropertyOf skos::narrower)
  (rdfs:subPropertyOf skos::mappingRelation)
  (skos::definition (@ "skos:narrowMatch is used to state a
  hierarchical mapping link between two conceptual resources in
  different concept schemes" "en")))

(defProperty skos::broadMatch (rdf:type owl:ObjectProperty)
  (rdfs:label (@ "has broader match" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (owl:inverseOf skos::narrowMatch)
  (rdfs:subPropertyOf skos::broader)
  (rdfs:subPropertyOf skos::mappingRelation)
  (skos::definition (@ "Used to state a hierarchical mapping link between two conceptual resources in different concept schemes" "en")))

(defProperty skos::closeMatch (rdf:type owl:SymmetricProperty)
  (rdfs:label (@ "has close match" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:subPropertyOf skos::mappingRelation)
  (skos::definition (@ "skos:closeMatch is used to link two concepts
  that are sufficiently similar that they can be used interchangeably
  in some information retrieval applications. In order to avoid the
  possibility of 'compound errors' when combining mappings across more
  than two concept schemes, skos:closeMatch is not declared to be a
  transitive property" "en")))

(defProperty skos::exactMatch (rdf:type owl:ObjectProperty)
  (rdf:type owl:TransitiveProperty)
  (rdf:type owl:SymmetricProperty)
  (rdfs:label (@ "has exact match" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:subPropertyOf skos::closeMatch)
  (rdfs:comment (@ "skos:exactMatch is disjoint with each of the
  properties skos:broadMatch and skos:relatedMatch" "en"))
  (skos::definition (@ "skos:exactMatch is used to link two concepts,
  indicating a high degree of confidence that the concepts can be used
  interchangeably across a wide range of information retrieval
  applications. skos:exactMatch is a transitive property, and is a
  sub-property of skos:closeMatch" "en")))

(defProperty skos::related (rdf:type owl:SymmetricProperty)
  (rdfs:label (@ "has related" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:subPropertyOf skos::semanticRelation)
  (rdfs:comment (@ "skos:related is disjoint with skos:broaderTransitive" "en"))
  (skos::definition (@ "Relates a concept to a concept with which
  there is an associative semantic relationship" "en")))

(defProperty skos::relatedMatch (rdf:type owl:SymmetricProperty)
  (rdfs:label (@ "has related match" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:subPropertyOf skos::related)
  (rdfs:subPropertyOf skos::mappingRelation)
  (skos::definition (@ "skos:relatedMatch is used to state an
  associative mapping link between two conceptual resources in
  different concept schemes" "en")))

(defProperty skos::semanticRelation (rdf:type owl:ObjectProperty)
  (rdfs:label (@ "is in a semantic relation with" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:domain skos::Concept)
  (rdfs:range  skos::Concept)
  (skos::definition (@ "Links a concept to a concept related by meaning" "en"))
  (skos::scopeNote (@ "This property should not be used directly, but
  as a super-property for all properties denoting a relationship of
  meaning between concepts" "en")))

(defProperty skos::mappingRelation (rdf:type owl:ObjectProperty)
  (rdfs:label (@ "is in a mapping relation with" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:subPropertyOf skos::semanticRelation)
  (skos::definition (@ "Relates two concepts coming, by convention,
  from different schemes, and that have comparable meanings" "en"))
  (rdfs:comment (@ "These concept mapping relations mirror semantic
  relations, and the data model defined below is similar (with the
  exception of skos:exactMatch) to the data model defined for semantic
  relations. A distinct vocabulary is provided for concept mapping
  relations, to provide a convenient way to differentiate links within
  a concept scheme from links between concept schemes. However, this
  pattern of usage is not a formal requirement of the SKOS data model,
  and relies on informal definitions of best practice" "en")))

(defProperty skos::broaderTransitive (rdf:type owl:TransitiveProperty)
  (rdfs:label (@ "has broader transitive" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:subPropertyOf skos::semanticRelation)
  (owl:inverseOf skos::narrowerTransitive)
  (skos:definition (@ "skos:broaderTransitive is a transitive
  superproperty of skos:broader" "en"))
  (skos::scopeNote (@ "By convention, skos:broaderTransitive is not
  used to make assertions. Rather, the prop'erties can be used to draw
  inferences about the transitive closure of the hierarchical
  relation, which is useful e.g. when implementing a simple query
  expansion algorithm in a search application" "en")))

(defProperty skos::narrowerTransitive (rdf:type owl:TransitiveProperty)
  (rdfs:label (@ "has narrower transitive" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:subPropertyOf skos::semanticRelation)
  (owl:inverseOf skos::broaderTransitive)
  (skos::definition (@ "skos:narrowerTransitive is a transitive
  superproperty of skos:narrower" "en"))
  (skos::scopeNote (@ "By convention, skos:narrowerTransitive is not
  used to make assertions. Rather, the properties can be used to draw
  inferences about the transitive closure of the hierarchical
  relation, which is useful e.g. when implementing a simple query
  expansion algorithm in a search application" "en")))

(defProperty skos::memberList (rdf:type owl:FunctionalProperty)
  (rdfs:label (@ "has member list" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:domain skos::OrderedCollection)
  (rdfs:range  rdf:List)
  (rdfs:comment (@ "For any resource, every item in the list given as
  the value of the skos:memberList property is also a value of the
  skos:member property" "en"))
  (skos::definition (@ "Relates an ordered collection to the RDF list
  containing its members" "en")))
  
(defProperty skos::hiddenLabel (rdf:type owl:AnnotationProperty)
  (rdfs:label (@ "hidden label" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:subPropertyOf rdfs:label)
  (rdfs:comment (@ "skos:prefLabel, skos:altLabel and skos:hiddenLabel
  are pairwise disjoint properties" "en"))
  (rdfs:comment (@ "The range of skos:hiddenLabel is the class of RDF plain literals" "en"))
  (skos::definition (@ "A lexical label for a resource that should be
  hidden when generating visual displays of the resource, but should
  still be accessible to free text search operations" "en")))

(defProperty skos::prefLabel (rdf:type owl:AnnotationProperty)
  (rdfs:label (@ "hidden label" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:subPropertyOf rdfs:label)
  (rdfs:comment (@ "skos:prefLabel, skos:altLabel and skos:hiddenLabel
  are pairwise disjoint properties" "en"))
  (rdfs:comment (@ "The range of skos:prefLabel is the class of RDF plain literals" "en"))
  (rdfs:comment (@ "A resource has no more than one value of skos:prefLabel per language tag" "en"))
  (skos::definition (@ "The preferred lexical label for a resource, in a given language" "en")))

(defProperty skos::altLabel (rdf:type owl:AnnotationProperty)
  (rdfs:label (@ "alternative label" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:subPropertyOf rdfs:label)
  (rdfs:comment (@ "skos:prefLabel, skos:altLabel and skos:hiddenLabel
  are pairwise disjoint properties" "en"))
  (rdfs:comment (@ "The range of skos:altLabel is the class of RDF plain literals" "en"))
  (skos::example (@ "Acronyms, abbreviations, spelling variants, and
  irregular plural/singular forms may be included among the
  alternative labels for a concept. Mis-spelled terms are normally
  included as hidden labels (see skos:hiddenLabel)" "en"))
  (skos::definition (@ "An alternative lexical label for a resource" "en")))

(defProperty skos::note (rdf:type owl:AnnotationProperty)
  (rdfs:label (@ "Note" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (skos::definition (@ "A general note, for any purpose" "en"))
  (skos::scopeNote (@ "This property may be used directly, or as a
  super-property for more specific note types" "en")))

(defProperty skos::changeNote (rdf:type owl:AnnotationProperty)
  (rdfs:label (@ "Change Note" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:subPropertyOf skos::note)
  (skos::definition (@ "A note about a modification to a concept" "en")))

(defProperty skos::historyNote (rdf:type owl:AnnotationProperty)
  (rdfs:label (@ "History Note" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:subPropertyOf skos::note)
  (skos:definition (@ "A note about the past state/use/meaning of a concept" "en")))

(defProperty skos::editorialNote (rdf:type owl:AnnotationProperty)
  (rdfs:label (@ "Editorial Note" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:subPropertyOf skos::note)
  (skos::definition (@ "A note for an editor, translator or maintainer of the vocabulary" "en")))

(defProperty skos::scopeNote (rdf:type owl:AnnotationProperty)
  (rdfs:label (@ "Scope Note" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:subPropertyOf skos::note)
  (skos::definition (@ "A note that helps to clarify the meaning
  and/or the use of a concept" "en")))

(defProperty skos::example (rdf:type owl:AnnotationProperty)
  (rdfs:label (@ "Example" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:subPropertyOf skos::note)
  (skos::definition (@ "An example of the use of a concept" "en")))

(defProperty skos::definition (rdf:type owl:AnnotationProperty)
  (rdfs:label (@ "Definition" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:subPropertyOf skos::note)
  (skos::definition (@ "A statement or formal explanation of the meaning of a concept" "en")))

(defProperty skos::inScheme (rdf:type owl:ObjectProperty)
  (rdfs:label (@ "is in scheme" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:range skos::ConceptScheme)
  (skos::definition (@ "Relates a resource (for example a concept) to a
  concept scheme in which it is included" "en"))
  (skos::scopeNote (@ "A concept may be a member of more than one concept scheme" "en")))

(defProperty skos::topConceptOf (rdf:type owl:ObjectProperty)
  (rdfs:label (@ "is top concept in scheme" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:domain skos::Concept)
  (rdfs:range  skos::ConceptScheme)
  (owl:inverseOf skos::hasTopConcept)
  (rdfs:subPropertyOf skos::inScheme)
  (skos::definition (@ "Relates a concept to the concept scheme that
  it is a top level concept of" "en")))

(defProperty skos::hasTopConcept (rdf:type owl:ObjectProperty)
  (rdfs:label (@ "has top concept" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:domain skos::ConceptScheme)
  (rdfs:range  skos::Concept)
  (owl:inverseOf skos::topConceptOf)
  (skos::definition (@ "Relates, by convention, a concept scheme to a
  concept which is topmost in the broader/narrower concept hierarchies
  for that scheme, providing an entry point to these
  hierarchies" "en")))

(defProperty skos::notation (rdf:type owl:DatatypeProperty)
  (rdfs:label (@ "notation" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (skos::definition (@ "A notation, also known as classification code,
  is a string of characters such as 'T58.5' or '303.4833' used to
  uniquely identify a concept within the scope of a given concept
  scheme" "en"))
  (skos::scopeNote (@ "By convention, skos:notation is used with a
  typed literal in the object position of the triple" "en")))

(cl:shadow "member" (cl:find-package :skos))

(defProperty skos::member (rdf:type owl:ObjectProperty)
  (rdfs:label (@ "has member" "en"))
  (rdfs:isDefinedBy (uri "http://www.w3.org/2004/02/skos/core"))
  (rdfs:domain skos::Collection)
  (rdfs:range (owl:Class (owl:unionOf skos::Concept)
                         (owl:unionOf skos::Collection)))
  (skos::definition (@ "Relates a collection to one of its members" "en")))

;;;;;
;; Local Variables:
;; mode: outline-minor 
;; indent-tabs: nil
;; outline-regexp: ";;[;]+"
;; End:


