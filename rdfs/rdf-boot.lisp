;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; Rdf Boot module
;;;
;;; IT Program Project in Japan: 
;;;    Building Operation-Support System for Large-scale System using IT
;;;
;;; This code is written by Seiji Koide at Galaxy Express Corporation, Japan,
;;; for the realization of the MEXT IT Program in Japan.
;;;
;;; Copyright (c) 2002-2005 Galaxy Express Corporation
;;; 
;;; Copyright (c) 2007-2009 Seiji Koide
;;;
;; History
;; -------
;; 2009.10.20    metaRDFSclass is deleted. Instead, _rdfsClass is used and it is classified to meta-galaxy.
;; 2009.09.04    name RDFSclass is changed to _rdfsClass.
;; 2009.08.29    The trick for membership loop of rdfs:Class is found.
;; 2008.11.05    Slot definition parts are moved from here to SlotDef.
;; 2007.03.21    comsmos-class is renamed to metaRDFSclass.
;; 2007.03.19    galaxy-class is renamed to RDFSclass.
;; 2007.03.12    file name is changed to rdfboot.
;; 2005.03.08    boot file is separated from rdfkernel
;; 2004.12.22    booting sequence is drastically changed
;; 2004.12.17    property direct-instances are created
;; 2004.12.16    shadow-class is created
;; 2004.07.28    Rdfs file is split out into RdfsKernel and RdfsCore.
;; 2004.03.13    name is provieded for ID and rdfs:label is set up as just label for print out.
;; 2004.01.14    rdf parser is placed in rdf module and package declarations are moved there.
;; 2004.01.09    parse-rdf for XML parser is prepared.
;; 2003.11.29    Utilities are separated to utils.cl file.
;; 2002.08.15    File created
;;; ==================================================================================
;;; The aim of this file is to establish the complex relation of meta-circularity in RDF 
;;; incrementally step by step. Loading this file yealds the following hierachical structure 
;;; at the end.
;;; ----------------------------------------------------------------------------------
;;;                                          ........................
;;;                                          :                      :
;;;                                          :                    _rdfsClass
;;;                                          :              ...../...:
;;;                                          :              :   /    :
;;; cl:standard-class -- meta-galaxy --------:--------rdfs:Class---shadow-class
;;;                            : :...........:       /      :
;;;                            :               ...../.......:
;;;                            :               :   /        :
;;;  cl:standard-object -- galaxy -- rdfs:Resource --- rdf:Property
;;;
;;;  ---, /  super/sub class relation, the direction of super is right to left.
;;;  ..., :  class/instance relation, the direction of class is upward and left to right.
;;; ----------------------------------------------------------------------------------
;;;
;;; The twisted relation between rdfs:Class and _rdfsClass, that is, rdfs:Class is a super 
;;; class and an instance of _rdfsClass produces a trick of (cl:typep rdfs:Class rdfs:Class).
;;; Note that every method defined at rdfs:Class affects rdfs:Class itself. Therefore, the 
;;; world is closed by the membership loop of rdfs:Class just like cl:standard-class in CLOS.
;;;
;;; The twisted relation between rdfs:Resource and rdfs:Class yields semantics of RDFS, that is, 
;;; (cl:typep rdfs:Resource rdfs:Class) and (cl:typep rdfs:Resource rdfs:Resource).
;;;
;;; The method <class-direct-instances>, which maintains direct instances of a class, is 
;;; inherited to rdfs:Class and _rdfsClass. So, subclasses of rdfs:Resource and rdfs:Class, 
;;; including rdfs:Class itself can hold their instances.

;; (cl:provide :rdfboot)

;; (eval-when (:execute :load-toplevel :compile-toplevel)
;;   (require :rdfspackages)
;;   (require :slotdef)) ; end of eval-when

(defpackage :gx
  (:export |rdfs:Resource| metaRDFSclass  _rdfsClass *base-uri* *reify-p*
           nodeID? nodeID2symbol mclasses)
  (:export property?     subPropertyOf)
  (:export typep         class-direct-instances))

(in-package :gx)

;;;
;;;; First of all, 
;;; we make skeltons that provide subtyping and metaclassing. 
;;;
;;;; galaxy & meta-galaxy
;;; Meta-galaxy is a superclass of rdfs:Class. 
;;; Galaxy is a superclass of rdfs:Resource. 
;;; Galaxy class is needed for registration of class-instance relation. 

(defclass meta-galaxy (standard-class)
  ((direct-instances :initarg :direct-instances :initform nil 
                     :accessor class-direct-instances))
  (:documentation "This metaclass is a class of galaxy. This metaclass provides method class-direct-instances."))

(defclass galaxy ()
  ((mclasses :initarg :mclasses :initform nil :accessor mclasses)
   (type-tag :initform nil :accessor resource-type-tag))
  (:metaclass meta-galaxy)
  (:documentation "This class is needed to maintain mclasses."))

(defmethod name ((class meta-galaxy))
  "returns a name of <class>, if it is named, otherwise nil."
  (slot-value class 'excl::name))

(defmethod name ((object galaxy))
  "returns a name of <object>, if it is named, otherwise nil."
  (slot-value object 'excl::name))

(defmethod (setf name) (symbol (class meta-galaxy) )
  "exports <symbol> for QName."
  (unless (nodeID? symbol)
    (setf (slot-value class 'excl::name) symbol))
  (export-as-QName symbol)
  (setf (symbol-value symbol) class))

(defmethod (setf name) (symbol (object galaxy) )
  "exports <symbol> for QName."
  (unless (nodeID? symbol)
    (setf (slot-value object 'excl::name) symbol))
  (export-as-QName symbol)
  (setf (symbol-value symbol) object))

;;;
;;;; rdfs:Resource
;;; At initial stage of booting, kernel classes are defined without slots to let class-changing easy.

(defparameter rdfs:Resource
  (defclass rdfs:Resource (galaxy) ()
    (:metaclass meta-galaxy)
    (:documentation "every resource in RDF(S) universe including classes is an instance of 
rdfs:Resource."))
  "rdfs:Resource is the top class in the RDF universe, but subclass of galaxy actually.")

(defmethod print-object ((obj rdfs:Resource) stream)
  (cond ((not (slot-exists-p obj 'excl::name))
         (call-next-method))
        ((and (slot-boundp obj 'excl::name)
              (slot-value obj 'excl::name))
         (print-unreadable-object (obj stream :type t)
           (prin1 (slot-value obj 'excl::name) stream)))
        (t (print-unreadable-object (obj stream :type t)
             (prin1 :anonymous stream)))))

;;;
;;;; _rdfsClass & rdfs:Class
;;; _rdfsClass is invented in order to realize the rdfs:Class membership loop. Namely, the class of 
;;; rdfs:Class is rdfs:Class itself in RDF(S) semantics. In SWCLOS rdfs:Class is actually the 
;;; class of rdfs:Class because rdfs:Class is a superclass of _rdfsClass and _rdfsClass is a class of 
;;; rdfs:Class. Thus, all methods are for instances of rdfs:Class is effective for rdfs:Class itself. 

(defclass _rdfsClass (rdfs:Resource meta-galaxy) ()
  (:metaclass meta-galaxy)) ; this is redefined later.

(defparameter rdfs:Class
  (defclass rdfs:Class (rdfs:Resource meta-galaxy) ()
    (:metaclass _rdfsClass)
    (:documentation "This is rdfs:Class, and it is a class of all classes in RDF(S) universe."))
  "This is rdfs:Class and it is a class of all classes in RDF(S) universe.")

;; print-object is inherited from rdfs:Resource.

;;;
;;;; Now, rdfs:Class is created. So, change the class of rdfs:Resource to rdfs:Class.
;;;
(cl:change-class (find-class 'rdfs:Resource) 'rdfs:Class)

(excl:without-redefinition-warnings
    (defclass _rdfsClass (rdfs:Class) ()
      (:metaclass meta-galaxy)
      (:documentation "This class is for meta-circularity of rdfs:Class, by making the twisted 
relation to rdfs:Resource")))

;;;
;;;; OK. The minimal skelton is completed.
;;;

;;;
;;;; Pseudo class of rdfs:Resource is defined.
;;; In order to work around wasteful slot definitions during forward-referencing, we invented a trick 
;;; of setting pseudo class of rdfs:Resource. If there would be no information on a forward-referenced 
;;; object, the  object is defined as an instance of |rdfs:Resource| rather than rdfs:Resource. 
;;; Or else, when a slot would be demanded to a tentatively created object in forward-referencing, 
;;; it would causes the slot definitions at rdfs:Resource. Then, when a proper definition comes up, 
;;; the slot definitions would be created at the proper class. As a result, rdfs:Resource would 
;;; become to have wasteful slot definitions for many objects in the universe. To work around this 
;;; phenomenum, |rdfs:Resource| is used instead of rdfs:Resource in forward-referencing. 
;;; |rdfs:Resource| prevents to create wasteful slots at rdfs:Resource instances. 
;;;

(defparameter |rdfs:Resource|
  (defclass |rdfs:Resource| (rdfs:Resource) () (:metaclass rdfs:Class))
  "|rdfs:Resource| is a pseudo rdfs:Resource in order to work around the slot
inheritance of temporal definition.
The rule of rdf4 entails a subject and an object as an instance of
rdfs:Resource. However the proactive application of this rule causes
the slot definition inheritance to the instances of rdfs:Class and
rdfs:Datatype and amounts to wasteful slots in every objects. To cope
with this problem, rdf4 treats |rdfs:Resource| metaobject instead of
rdfs:Resource.")

;;; 
;;;; Then, we proceed slot definitions.
;;;

;;;
;;; <mop:direct-slot-definition-class> returns a direct slot definition class for target class. 
;;; This method is customized to return an appropriate slot definition class in SWCLOS, i.e., 
;;; <Property-direct-slot-definition> or <OwlProperty-direct-slot-definition>. Namely, if an 
;;; indicator in initargs is not a keyword, it must be a property name. The name of rdf, rdfs, 
;;; and owl propertes are embeded in this routine. In other case, if the domain includes 'owl:Restriction', 
;;; then Property-direct-slot-definition is returned. If the property is an instance of 'owl:ObjectProperty', 
;;; then OwlProperty-direct-slot-definition is returned, else if the defalut value is returned.

(defmethod mop:direct-slot-definition-class ((class rdfs:Class) &rest initargs)
  "If <initargs> include non-keyword indicators for slot initarg or include a property name as slot name, 
    then returns Property-direct-slot-definition or OwlProperty-direct-slot-definition metaobject."
  ;; this code is shared by OWL.
  (declare (optimize (speed 3) (safety 0)))
  (if (keywordp (car (getf initargs :initargs))) (call-next-method)
    (case (getf initargs :name)
      ((rdf:about rdf:ID xml:lang) (call-next-method))
      ((rdf:type rdfs:subClassOf rdfs:label rdfs:isDefinedBy rdfs:comment 
                 rdfs:domain rdfs:range rdfs:subPropertyOf rdfs:member rdf:value 
                 rdf:first rdf:rest rdf:object rdf:subject rdf:predicate  
                 owl:oneOf owl:intersectionOf owl:unionOf 
                 owl:allValuesFrom owl:hasValue owl:someValuesFromRestriction 
                 owl:cardinality owl:maxCardinality owl:minCardinality
                 owl:onProperty owl:distinctMembers owl:differentFrom owl:sameAs 
                 owl:equivalentProperty owl:equivalentClass 
                 owl:complementOf owl:disjointWith
                 owl:inverseOf)           ; Note owl:inverseOf is an instance of rdf:Property
       (find-class 'gx::Property-direct-slot-definition))
      (otherwise
       (cond ((property? (getf initargs :name))
              (let* ((prop (symbol-value (getf initargs :name)))
                     (domains (and (slot-boundp prop 'rdfs:domain) (slot-value prop 'rdfs:domain))))
                (cond ((and (consp domains)
                            (member 'owl:Restriction domains :key #'name))
                       (find-class 'gx::Property-direct-slot-definition))
                      ((and domains (eq (cg::name domains) 'owl:Restriction)
                       (find-class 'gx::Property-direct-slot-definition)))
                      ((and (find-class 'owl:ObjectProperty nil) (cl:typep prop 'owl:ObjectProperty))
                       (find-class 'OwlProperty-direct-slot-definition))
                      (t (find-class *default-slot-definition-class*)))))
             (t ;; non-keyword symbol should be a role name.
              (cond ((cl:typep (symbol-value (getf initargs :name)) 'owl:ObjectProperty)
                     (find-class 'OwlProperty-direct-slot-definition))
                    (t (find-class *default-slot-definition-class*)))))))))

;;;
;;;; How to Compute a Type Value in Effective Slot Definition
;;;
;;; Since the function of gx:subtypep in RDF is the same as that of cl:subtypep, the 
;;; computation of type option in <compute-effective-slot-definition-initargs> for excl::std-class is 
;;; also useful for the effective slot definition for rdf properties. Note that the ACL original 
;;; algorithm for type value collection collects the type value in direct-slot-definitions at every 
;;; superclass of the target class, and makes the conjunction of them. See the following example.
;;; ----------------------------------------------------------------------------------
;;; (defclass C1 () ((s :type cl:number)))
;;; (defclass C2 (C1) ((s :type cl:float)))
;;; (defclass C3 (C1) ((s :type cl:integer)))
;;; (defclass C4 (C3) ((s :type cl:fixnum)))
;;; (defclass C5 (C4 C2) ())
;;; (mop:slot-definition-type (first (mop:compute-slots (find-class 'C5))))
;;;  -> (and float fixnum)
;;; ----------------------------------------------------------------------------------
;;; This computational result by native routine of ACL is sound, if members of conjunct are not 
;;; disjoint each other, whereas it might be not minimal expression as conjunction, since ACL does 
;;; not reduce the result such as conjunctive normal form (CNF). However, if you take care of the 
;;; disjointness between cl:float and cl:fixnum, then such result will be nonsense. In SWCLOS, the 
;;; notion of disjointness is taken care for not only OWL universe but also RDF universe. In 
;;; SWCLOS, the clash by disjointness is directed as follows.
;;; ----------------------------------------------------------------------------------
;;; (defConcept C1 (rdfs:subClassOf (owl:Restriction (owl:onProperty s)
;;;                                    (owl:allValuesFrom xsd:decimal))))
;;; (defConcept C2 (rdfs:subClassOf (owl:Restriction (owl:onProperty s)
;;;                                    (owl:allValuesFrom xsd:float))))
;;; (defConcept C3 (rdfs:subClassOf (owl:Restriction (owl:onProperty s)
;;;                                    (owl:allValuesFrom xsd:integer))))
;;; (defConcept C4 (rdfs:subClassOf (owl:Restriction (owl:onProperty s)
;;;                                    (owl:allValuesFrom xsd:short))))
;;; (defConcept C5 (rdfs:subClassOf C4 C2))
;;; (mop:slot-definition-type (car (mop:compute-slots C5)))
;;; -> Error: Disjoint pair #<forall s xsd:short> and #<forall s xsd:float> found in slot 
;;;    inheritance computation of #<rdfs:Class C5>.
;;; ----------------------------------------------------------------------------------
;;;
;;; In this imlementation of <compute-effective-slot-definition-initargs> in SWCLOS, 
;;; we let the native CLOS routine compute the type option. 
;;; After the computation for CLOS native effective slots definition, the subject-type option is 
;;; filled with the value of <class> parameter in <compute-effective-slot-definition-initargs>.
;;; 
;;; However, <compute-effective-slot-definition-initargs> is redefined in OWL module. 
;;; In OWL system, the satisfiability among conjunctions in type option is checked.  
;;; See <compute-effective-slot-definition-initargs> in OWL system.
;;;

(defmethod excl::compute-effective-slot-definition-initargs ((class rdfs:Class) direct-slotds)
  "see above"
  (declare (optimize (speed 3) (safety 0)))
  (let ((initargs (call-next-method)))
    (cond ((member-if #'property-direct-slotd-p direct-slotds)
           ;; if a slotd is property slotd, add subject-type option.
           `(:subject-type ,class ,@initargs))
          (t initargs))))

;;;
;;; If <initargs> in making an effective-slot-definition includes :subject-type keyword, the slot-definition must be 
;;; Property-effective-slot-definition. So, <mop:effective-slot-definition-class> methods returns the class metaobject. 
;;; Then, CLOS system takes care of all after.

(defmethod mop:effective-slot-definition-class ((class rdfs:Class) &rest initargs)
  "see above"
  (declare (optimize (speed 3) (safety 0)))
  (cond ((member :subject-type initargs)
         (find-class 'gx::Property-effective-slot-definition))
        (t (call-next-method))))

;;;; Hierarchy and Relation around Property
;;; An rdf property is an instance of rdf:Property. An rdf property as rdf-object has a slot 
;;; <property-slotds> for book-keeping, which holds a list of slot definitions on the property 
;;; (e.g., eslotd1, eslotd2 eslotd3 for rdfs:label, see below). Each slot definition keeps a class of 
;;; subjective object in subject-type option. Therefore, we can retrieve every triples with 
;;; respect to a property (called the extension of property). The object in triple is obtained by 
;;; accessing slot value to the object with the predicate (slot-name) in the triple.
;;; ----------------------------------------------------------------------------------
;;;
;;; rdfs:Resource ---------------------------------- rdf:Property
;;;                                                       :
;;;                                              ex.  rdfs:label
;;;                                                          |
;;; Property-effective-slot-definition                       | <-- property-slotds
;;;                    :.....................................|....
;;;                                                          |   :
;;;                                                          +-(eslotd1 eslotd2 eslotd3)
;;;                                                                 |
;;;                                                                 | <-- subject-type
;;;                                                                 |
;;;                                                <a class which this eslotd is attached>
;;; ----------------------------------------------------------------------------------

;;;
;;; Note that the domain and range of owl:equivalentProperty is rdf:Property rather than 
;;; owl:ObjectProperty.
;;;

(defparameter rdf:Property
  (defclass rdf:Property (rdfs:Resource)
    ((slotds :initarg :slotds :initform ()
             :documentation "slotds keeps <domain property> pair."))
                               ; see rdf:Property Final
    (:metaclass rdfs:Class)
    (:documentation "an instance of rdf:Property has a registory for slotds."))
  "every property in RDF(S) is an instance of rdf:Property. An instance of this class
has a place holder for all related slot definitions.")

(reinitialize-instance (load-time-value rdf:Property)
                       :mclasses `(,(load-time-value rdfs:Class)))

(defmethod print-object ((obj rdf:Property) stream)
  (cond ((not (slot-exists-p obj 'excl::name))
         (call-next-method))
        ((slot-boundp obj 'excl::name)
         (print-unreadable-object (obj stream :type t)
           (prin1 (slot-value obj 'excl::name) stream)))
        (t (print-unreadable-object (obj stream :type t)
             (prin1 :anonymous stream)))))

;;;
;;;; rdfs:Class final
;;; rdfs:Class is reinitialized with slots. 
;;; I thank smh for teaching me to use 'reinitialize-instance'.

(reinitialize-instance (load-time-value rdfs:Class)
                       :direct-slots
                       `((:name rdfs:subClassOf
                          :initform nil
                          :initfunction ,(load-time-value #'excl::false)
                          :type ,(load-time-value rdfs:Class)
                          :initargs (rdfs:subClassOf)
                          ;:readers (superclass-of)
                          ;:writers ((setf superclass-of))
                                )))

;;;
;;;; rdfs:Datatype 
;;;

(defparameter rdfs:Datatype
  (defclass rdfs:Datatype (rdfs:Class) () (:metaclass rdfs:Class))
  "rdfs:Datatype is a subclass of and an instance of rdfs:Class.")

;;;
;;;; rdfs:Literal & rdf:XMLLiteral
;;;

(defparameter rdfs:Literal
  (defclass rdfs:Literal (rdfs:Resource) () (:metaclass rdfs:Class))
  "rdfs:Literal is a subclass of rdfs:Resource and an instance of rdfs:Class.")
(reinitialize-instance (load-time-value rdfs:Literal)
                       :mclasses `(,(load-time-value rdfs:Class)))

(defparameter rdf:XMLLiteral
  (defclass rdf:XMLLiteral (rdfs:Literal)
    ((value :initarg :value))
    (:metaclass rdfs:Datatype))
  "rdf:XMLLiteral is a subclass of rdfs:Literal and an instance of rdfs:Datatype.
   An instance has a value of XMLLiteral data.")

(defmethod value-of ((x rdf:XMLLiteral))
  "retrieves a value of XMLLiteral data."
  (slot-value x 'value))

(defmethod value-of (x)
  "otherwise returns itself."
  x)

;;;
;;;; rdfs:Resource final
;;; rdfs:Resource is reinitialized with slots.

(reinitialize-instance (load-time-value rdfs:Resource)
                       :direct-slots
                       `((:name excl::name :initform common-lisp:nil
                          :initfunction ,(load-time-value #'excl::false)
                          :initargs (:name))
                         (:name rdf:about :initargs (rdf:about))
                         (:name rdf:ID :initargs (rdf:ID))
                         (:name xml:lang :initform common-lisp:nil
                          :initfunction ,(load-time-value #'excl::false)
                          :initargs (xml:lang))
                         (:name rdfs:label :type ,(load-time-value rdfs:Literal)
                          :initargs (rdfs:label))
                         (:name rdfs:isDefinedBy :type ,(load-time-value rdfs:Resource)
                          :initargs (rdfs:isDefinedBy))
                         (:name rdfs:comment :type ,(load-time-value rdfs:Resource)
                                :initargs (rdfs:comment))
                         (:name rdf:type :type ,(load-time-value rdfs:Class)
                                :initargs (rdf:type)
                                :documentation "for bookkeeping user-definition"))
                       :mclasses `(,(load-time-value rdfs:Class)))

;;;
;;;; rdf:List
;;;

(defparameter rdf:List
  (defclass rdf:List (rdfs:Resource)
    ()
    (:metaclass rdfs:Class))
  "rdf:List is a subclass of rdfs:Resource and an instance of rdfs:Class.")

;;;
;;;; rdf:Property
;;; Now, here many properties are defined.

(defparameter rdfs:label
  (make-instance 'rdf:Property
    :name 'rdfs:label
    :mclasses `(,(load-time-value rdf:Property)))
  "may be used to provide a human-readable version of a resource's name.")

(defparameter rdfs:comment
  (make-instance 'rdf:Property
    :name 'rdfs:comment
    :mclasses `(,(load-time-value rdf:Property)))
  "may be used to provide a human-readable description of a resource.")

(defparameter rdfs:isDefinedBy
  (make-instance 'rdf:Property
    :name 'rdfs:isDefinedBy
    :mclasses `(,(load-time-value rdf:Property)))
  "is used to indicate a resource defining the subject resource. 
This property may be used to indicate an RDF vocabulary in which a resource is described.")

(defparameter rdfs:domain
  (make-instance 'rdf:Property
    :name 'rdfs:domain
    :mclasses `(,(load-time-value rdf:Property)))
  "is used to state that any resource that has a given property is an instance of one or more classes.")

(defparameter rdfs:range
  (make-instance 'rdf:Property
    :name 'rdfs:range
    :mclasses `(,(load-time-value rdf:Property)))
  "is used to state that the values of a property are instances of one or more classes.")

(defparameter rdfs:subClassOf
  (make-instance 'rdf:Property
    :name 'rdfs:subClassOf
    :mclasses `(,(load-time-value rdf:Property)))
  "is used to state that all the instances of one class are instances of another.")

(defparameter rdfs:subPropertyOf
  (make-instance 'rdf:Property
    :name 'rdfs:subPropertyOf
    :mclasses `(,(load-time-value rdf:Property)))
  " is used to state that all resources related by one property are also related by another.")

(reinitialize-instance (load-time-value rdfs:Class)
                       :direct-instances `(,(load-time-value rdf:Property)
                                           ,(load-time-value rdfs:Resource)
                                           ,(load-time-value rdfs:Class))
                       :mclasses `(,(load-time-value rdfs:Class)))

;; End of module
;; --------------------------------------------------------------------
;;;
;;; Seiji Koide Oct-20-2009
;;;
