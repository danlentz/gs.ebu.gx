;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-
;;;;;
;;;;; earl.lisp
;;;;;
;;;;;   implementation of the Evaluation and Report Language Ontology
;;;;;   http://www.w3.org/ns/earl#
;;;;;
;;;;; Author:      Dan Lentz, Lentz Intergalactic Softworks
;;;;; Maintainer:  <danlentz@gmail.com>
;;;;;

(in-package :gx-user)

(defpackage :ptr (:use)
  (:export :Pointer)
  (:documentation "http://www.w3.org/2009/pointers"))
(set-uri-namedspace-from-pkg (find-package :ptr))

(defpackage :earl (:use)
  (:export :Assertion :Assertor :Software :TestSubject :TestCriterion
    :TestRequirement :TestCase :TestResult :TestMode :OutcomeValue
    :Pass :Fail :CannotTell :NotApplicable :NotTested :info :test
    :subject :assertedBy :result :mode :mainAssertor :outcome
    :unknownMode :automatic :manual :semiAuto :undisclosed
    :inappllicable :failed :untested :cantTell :passed :pointer)
  (:documentation "http://www.w3.org/ns/earl#"))
(set-uri-namedspace-from-pkg (find-package :earl))

;;;
;;; EARL Ontology
;;;

(defIndividual earl::Ontology (rdf:type owl:Ontology)
  (rdf:about "http://www.w3.org/ns/earl#")
  (rdfs:label (@ "EARL 1.0 Schema" "en"))
  (dc::title (@ "Evaluation and Report Language" "en"))
  (rdfs:comment  (@ "Evaluation And Report Language (EARL) 1.0 Schema as defined by
http://www.w3.org/TR/EARL10-Schema" "en")))

;;;
;;; EARL Classes (Nodes)
;;;

(defConcept earl::Assertion (rdf:type owl:Class)
  (rdfs:label (@ "Assertion" "en"))
  (rdfs:comment
   (@ "a statement that embodies the results of a test" "en")))

(defConcept earl::Assertor (rdf:type owl:Class)
  (rdfs:label (@ "Assertor" "en"))
  (rdfs:comment
   (@ "an entity such as a person, a software tool, an organization, or any other grouping that carries out a test collectively" "en")))

(defConcept earl::Software (rdf:type owl:Class)
  (rdfs:label (@ "Software" "en"))
  (rdfs:comment
   (@ "any piece of software such as an authoring tool, browser, or evaluation tool" "en")))

(defConcept earl::TestSubject (rdf:type owl:Class)
  (rdfs:label (@ "Test Subject" "en"))
  (rdfs:comment
   (@ "the class of things that have been tested against some test criterion" "en")))

(defConcept earl::TestCriterion (rdf:type owl:Class)
  (rdfs:label (@ "Test Criterion" "en"))
  (rdfs:comment
   (@ "a testable statement, usually one that can be passed or failed" "en")))

(defConcept earl::TestRequirement (rdf:type owl:Class)
  (rdfs:label (@ "Test Requirement" "en"))
  (rdfs:comment
   (@ "a higher-level requirement that is tested by executing one or more sub-tests" "en")))

(defConcept earl::TestCase (rdfs:subClassOf earl::TestCriterion)
  (rdfs:label (@ "Test Case" "en"))
  (rdfs:comment
   (@ "an atomic test, usually one that is a partial test for a requirement" "en")))
  
(defConcept earl::TestResult (rdf:type owl:Class)
  (rdfs:label (@ "Test Result" "en"))
  (rdfs:comment
   (@ "the actual result of performing the test" "en")))

(defConcept earl::TestMode (rdf:type owl:Class)
  (rdfs:label (@ "Test Mode" "en"))
  (rdfs:comment
   (@ ">describes how a test was carried out" "en")))

(defConcept earl::OutcomeValue (rdf:type owl:Class)
  (rdfs:label (@ "Outcome Value" "en"))
  (rdfs:comment
   (@ "a discrete value that describes a resulting condition from carrying out the test" "en")))

(defConcept earl::Pass (rdfs::subClassOf earl::OutcomeValue)
  (rdfs:label (@ "Passed" "en"))
  (rdfs:comment
   (@ "the subject passed the test" "en")))

(defConcept earl::Fail (rdfs::subClassOf earl::OutcomeValue)
  (rdfs:label (@ "Failed" "en"))
  (rdfs:comment
   (@ "the subject failed the test" "en")))

(defConcept earl::CannotTell (rdfs::subClassOf earl::OutcomeValue)
  (rdfs:label (@ "Cannot Tell" "en"))
  (rdfs:comment
   (@ "it is unclear if the subject passed or failed the test" "en")))

(defConcept earl::NotApplicable (rdfs::subClassOf earl::OutcomeValue)
  (rdfs:label (@ "Not Applicable" "en"))
  (rdfs:comment
   (@ "the test is not applicable to the subject" "en")))

(defConcept earl::NotTested (rdfs::subClassOf earl::OutcomeValue)
  (rdfs:label (@ "Not Tested" "en"))
  (rdfs:comment
   (@ "the test has not been carried out" "en")))

;;;
;;; EARL Properties (Edges)
;;;

(defProperty earl::subject (rdf:type owl:ObjectProperty)
  (rdfs:label (@ "Subject" "en"))
  (rdfs:comment (@ "test subject of an assertion" "en"))
  (rdfs:domain earl::Assertion)
  (rdfs:range  earl::TestSubject))

(defProperty earl::assertedBy (rdf:type owl:ObjectProperty)
  (rdfs:label (@ "Asserted By" "en"))
  (rdfs:comment (@ "assertor of an assertion" "en"))
  (rdfs:domain earl::Assertion)
  (rdfs:range  earl::Assertor))

(defProperty earl::result (rdf:type owl:ObjectProperty)
  (rdfs:label (@ "Result" "en"))
  (rdfs:comment (@ "result of an assertion" "en"))
  (rdfs:domain earl::Assertion)
  (rdfs:range  earl::TestResult))

(defProperty earl::mode (rdf:type owl:ObjectProperty)
  (rdfs:label (@ "Mode" "en"))
  (rdfs:comment (@ "mode in which the test was performed" "en"))
  (rdfs:domain earl::Assertion)
  (rdfs:range  earl::TestMode))

(defProperty earl::mainAssertor (rdf:type owl:ObjectProperty) ;; TODO
  (rdfs:label (@ "Main Assertor" "en"))
  (rdfs:comment (@ "assertor that is primarily responsible for performing the test" "en"))
  (rdfs:domain earl::Assertor)
  (rdfs:range  earl::Assertor))

(defProperty earl::outcome (rdf:type owl:ObjectProperty)
  (rdfs:label (@ "Outcome" "en"))
  (rdfs:comment (@ "outcome of performing the test" "en"))
  (rdfs:domain earl::TestResult)
  (rdfs:range  earl::OutcomeValue))

(defProperty earl::pointer (rdf:type owl:ObjectProperty)
  (rdfs:label (@ "Pointer" "en"))
  (rdfs:comment (@ "location within a test subject that are most relevant to a test result" "en"))
  (rdfs:domain earl::TestResult)
  (rdfs:range  ptr::Pointer))

(defProperty earl::info (rdf:type owl:ObjectProperty)
  (rdfs:label (@ "Info" "en"))
  (rdfs:comment (@ "additional warnings or error messages in a human-readable form" "en"))
  (rdfs:domain earl::TestResult)
  (rdfs:range  rdfs:Literal))

(defProperty earl::test (rdf:type owl:ObjectProperty)
  (rdfs:label (@ "Test" "en"))
  (rdfs:comment (@ "test criterion of an assertion" "en"))
  (rdfs:domain earl::Assertion)
  (rdfs:range  earl::TestCriterion))

;;;
;;; EARL Instances
;;;

(defIndividual earl::unknownMode (rdf:type earl::TestMode)
  (dc::title (@ "Unknown" "en"))
  (dc::description
   (@ "where the testing process is unknown or undetermined" "en")))

(defIndividual earl::automatic (rdf:type earl::TestMode)
  (dc::title (@ "Automatic" "en"))
  (dc::description
   (@ "where the test was carried out automatically by the software tool and without any human intervention" "en")))

(defIndividual earl::manual (rdf:type earl::TestMode)
  (dc::title (@ "Manual" "en"))
  (dc::description
   (@ "where the test was carried out by human evaluators" "en")))

(defIndividual earl::semiAuto (rdf:type earl::TestMode)
  (dc::title (@ "Semi-Automatic" "en"))
  (dc::description
   (@ "where the test was partially carried out by software tools, but where human input or judgment was still required to decide or help decide the outcome of the test" "en")))

(defIndividual earl::undisclosed (rdf:type earl::TestMode)
  (dc::title (@ "Undisclosed" "en"))
  (dc::description
   (@ "where the exact testing process is undisclosed" "en")))

(defIndividual earl::inapplicable (rdf:type earl::NotApplicable)
  (dc::title (@ "Not Applicable" "en"))
  (dc::description
   (@ "the test is not applicable to the subject" "en")))

(defIndividual earl::failed (rdf:type earl::Fail)
  (dc::title (@ "Failed" "en"))
  (dc::description
   (@ "the subject failed the test" "en")))

(defIndividual earl::untested (rdf:type earl::NotTested)
  (dc::title (@ "Not Tested" "en"))
  (dc::description
   (@ "the test has not been carried out" "en")))

(defIndividual earl::cantTell (rdf:type earl::CannotTell)
  (dc::title (@ "Cannot Tell" "en"))
  (dc::description
   (@ "it is unclear if the subject passed or failed the test" "en")))

(defIndividual earl::passed (rdf:type earl::Pass)
  (dc::title (@ "Passed" "en"))
  (dc::description
   (@ "the subject passed the test" "en")))
  
;;;;;
;; Local Variables:
;; mode: outline-minor
;; indent-tabs: nil
;; outline-regexp: ";;[;]+"
;; End:
