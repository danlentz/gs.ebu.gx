;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; N-triple module
;;;
;;; IT Program Project in Japan: 
;;;          Building Operation-Support System for Large-scale System using IT
;;;
;;; Copyright © 2003 by Galaxy Express Corporation
;;;
;; History
;; -------
;; 2009.09.04    name RDFSclass is changed to _rdfsClass.
;; 2008.12.11    resource-p is renamed to rdf-objectp
;; 2003.08.3    File created
;;
;;; ==================================================================================

;; (cl:provide :ntriple)

;; (eval-when (:execute :load-toplevel :compile-toplevel)
;;   (require :rdfscore)
;;   )

(defpackage :gx
  (:export defTriple addTriple /. ./ get-triple
           superclasses-of subclasses-of))

(in-package :gx)

;;;
;;;; defTriple
;;;

(defmacro defTriple (subject predicate object)
  "defines a triple with forward-reference functionality."
  `(addTriple ',subject ',predicate ',object))

(defmacro /. (subject predicate object)
  "defines a triple with forward-reference functionality."
  `(addTriple ',subject ',predicate ',object))
(defmacro ./ (subject predicate object)
  "defines a triple with forward-reference functionality."
  `(addTriple ',subject ',predicate ',object))

;;;
;;;; Add Triple
;;;
;;; addTriple shows very convenient usage of method. 
;;; A triple of subject/predicate/object in various types, which may be undefined in forward reference, 
;;; are accepted and they are set up step by step using appropriate entailment rules piecewisely.
;;; 
;;; ----------------------------------------------------------------------------------
;;;        +-- t/URI/t
;;;        +-- t/sym/t
;;;        +-- URI/t/t
;;;        |                                   +-- sym/rdf:type/sym
;;;        |                                   +-- sym/rdf:type/cls
;;;        +-- sym/rsc/t                       +-- sym/rdfs:subPropertyOf/sym
;;; t/t/t -+-- sym/prop/t --+-- sym/prop/sym --+-- sym/rdfs:subPropertyOf/prop
;;;        |                +-- sym/prop/data  +-- sym/rdfs:subClassOf/sym
;;;        |                                   +-- sym/rdfs:subClassOf/cls
;;;        |
;;;        +-- rsc/prop/t --+-- rsc/prop/sym
;;;        |                +-- cls/prop/t ---+-- cls/prop/sym 
;;;        |                                  +--------------------- cls/rdfs:subClassOf/cls
;;;        +-- rsc/rsc/t
;;;
;;; ----------------------------------------------------------------------------------

(defgeneric addTriple (subject predicate object)
  (:documentation
   "adds a subject-predicate-object triple.")
  )

;;;
;;;; symbol rdf:type Class
;;;

(defmethod addTriple ((subject symbol) (predicate (eql rdf:type)) (object rdfs:Class))
  "This form is turned out to an <addClass> form, due to predicate rdf:type."
  (unless (and (boundp subject) (typep (symbol-value subject) object))
    (cond ((rdf-metaclass-p object)
           (addClass (list object) subject '() '()))
          (t (addInstance (list object) subject '())))))

;;;
;;;; symbol rdf:type <symbol> -->  symbol rdf:type <resource>
;;;
;;; Range constraint is used for proactive entailment for undefined <symbol>.
;;; See entaiment rule rdfs3.

(defmethod addTriple ((subject symbol) (predicate (eql rdf:type)) (object symbol))
  (unless (object? object)
    (let ((range (get-range predicate))) ;range == rdfs:Class
      (warn "Entail in ~S rdf:type ~S:~%..... ~S rdf:type ~S." 
        subject object object (name range))
      (addTriple object rdf:type range)))
  (addTriple subject predicate (symbol-value object)))

(defmethod addTriple (subject (predicate (eql rdf:type)) object)
  (error "Triple input error: ~S ~S ~S." subject predicate object))

;;;
;;;; Class rdfs:subClassOf Class
;;;

(defmethod superclasses-of ((object rdfs:Class))
  (mop:class-direct-superclasses object))
(defmethod subclasses-of ((object rdfs:Class))
  (mop:class-direct-subclasses object))

(defmethod addTriple ((subject rdfs:Class) (predicate (eql rdfs:subClassOf)) (object rdfs:Class))
  ;; if subject is already subclass of object, nothing done.
  (if (cl:subtypep subject object) subject
    (let ((supers (superclasses-of subject)))
      (setq supers (most-specific-concepts (cons object supers)))
      (addClass (list (class-of subject)) (name subject) supers '()))))

;;;
;;;; <symbol> rdfs:subClassOf Class  -->  <resource> rdfs:subClassOf Class
;;;

(defmethod addTriple ((subject symbol) (predicate (eql rdfs:subClassOf)) (object rdfs:Class))
  (cond ((object? subject)
         (addTriple (symbol-value subject) predicate object))
        (t (let ((domain (get-domain predicate)))
             (warn "Entail in ~S rdfs:subClassOf ~S:~%..... ~S rdf:type ~S." 
               subject object subject (name domain))
             (addTriple subject rdf:type domain)
             (addTriple (symbol-value subject) predicate object)))))

;;;
;;;; symbol rdfs:subClassOf <symbol> -->  symbol rdfs:subClassOf <resource>
;;;

(defmethod addTriple ((subject symbol) (predicate (eql rdfs:subClassOf)) (object symbol))
  (cond ((property? object)
         (error "Range violation: ~S for rdfs:subClassOf range." (symbol-value object)))
        ((object? object)
         (addTriple subject predicate (symbol-value object)))
        ((call-next-method))))

;;;
;;;; Property rdfs:subPropertyOf Property
;;;

(defun strict-abst-property-p (abst spec)
  (strict-subproperty-p spec abst nil))

(defun most-specific-property (absts)
  (let ((l (remove-duplicates absts)))    ; eql should be assured
    (set-difference l l :test #'strict-abst-property-p)))

(defmethod addTriple ((subject rdf:Property) (predicate (eql rdfs:subPropertyOf)) (object rdf:Property))
  ;; if subject is already subproperty of object, nothing done.
  (if (subproperty-p subject object) subject
    (let ((supers (slot-value subject 'rdfs:subPropertyOf)))
      (setq supers (most-specific-property (cons object supers)))
      (addInstance (class-of subject) (name subject) `((rdfs:subPropertyOf ,supers))))))

;;;
;;;; <symbol> rdfs:subPropertyOf Property  -->  <resource> rdfs:subPropertyOf Property
;;;

(defmethod addTriple ((subject symbol) (predicate (eql rdfs:subPropertyOf)) (object rdf:Property))
  (cond ((object? subject)
         (addTriple (symbol-value subject) predicate object))
        (t (let ((domain (get-domain predicate)))
             (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
               subject (name predicate) object subject (name domain))
             (addTriple subject rdf:type domain)
             (addTriple (symbol-value subject) predicate object)))))

;;;
;;;; symbol rdfs:subPropertyOf <symbol> -->  symbol rdfs:subPropertyOf <resource>
;;;

(defmethod addTriple ((subject symbol) (predicate (eql rdfs:subPropertyOf)) (object symbol))
  (cond ((property? object)
         (addTriple subject predicate (symbol-value object)))
        ((object? object)
         (error "Range violation ~S for rdfs:subPropertyOf" object))
        ((call-next-method))))

(defmethod addTriple (subject (predicate (eql rdfs:subPropertyOf)) object)
  (error "Triple input error: ~S ~S ~S." subject predicate object))

;;;
;;;; Class Property <symbol> --> Class Property <resource>
;;;

(defmethod addTriple ((subject rdfs:Class) (predicate rdf:Property) (object symbol))
  (cond ((object? object)
         (addTriple subject predicate (symbol-value object)))
        (t (let ((range (or (get-range predicate) rdfs:Class)))
             (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
               subject (name predicate) object object (name range))
             (addTriple object rdf:type range))
           (addTriple subject predicate (symbol-value object)))))

;;;
;;;; Resource Property <symbol> --> Resource Property <resource>
;;;

(defmethod addTriple ((subject rdfs:Resource) (predicate rdf:Property) (object symbol))
  (cond ((object? object)
         (addTriple subject predicate (symbol-value object)))
        (t (let ((range (or (get-range predicate) |rdfs:Resource|)))
             (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
               subject (name predicate) object object (name range))
             (addTriple object rdf:type range)
             (addTriple subject predicate (symbol-value object))))))

;;;
;;;; Class Property t
;;;
          
(defmethod addTriple ((subject rdfs:Class) (predicate rdf:Property) object)
  (let ((domains (get-domain predicate)))
    (cond ((typep subject domains)
           (addClass (list (class-of subject)) subject '() `((,(name predicate) ,object))))
          ((atom domains)
           (warn "Domain Entail: ~S rdf:type ~S." (name domains) object)
           (addInstance domains (name subject) `((,(name predicate) ,object))))
          ((error 'domain-condition-unsatisfiable
             :format-control
             "CHECK DOMAIN of ~S to ~#[ none~; ~S~; ~S and ~S~:;~@{~#[~; and~] ~S~^,~}~]."
             :format-arguments `(,subject ,@(mapcar #'get-form (cdr domains))))))))
;;;
;;;; Resource Property t
;;;

(defmethod addTriple ((subject rdfs:Resource) (predicate rdf:Property) object)
  ;(format t "~%Adding ~S ~S ~S ." subject predicate object)
  (let ((name (if (anonymous-p subject) (make-unique-nodeID "aa") (name subject)))
        (domains (get-domain predicate)))
    (setf (symbol-value name) subject)
    (cond ((null domains)
           (addInstance (class-of subject) name `((,(name predicate) ,object))))
          ((typep subject domains)
           (addInstance (class-of subject) name `((,(name predicate) ,object))))
          ((atom domains)
           (warn "Domain Entail: ~S rdf:type ~S." (name domains) object)
           (addInstance domains name `((,(name predicate) ,object))))
          ((error 'domain-condition-unsatisfiable
             :format-control
             "CHECK DOMAIN of ~S to ~#[ none~; ~S~; ~S and ~S~:;~@{~#[~; and~] ~S~^,~}~]."
             :format-arguments `(,subject ,@(mapcar #'get-form (cdr domains))))))))

(defmethod addTriple ((subject rdfs:Resource) (predicate rdfs:Resource) object)
  (addTriple subject (change-class predicate rdf:Property) object))

(defun collect-domaind (slots)
  "collects domain information from properties in <slots>."
  (loop for slot in slots with domain
      when (setq domain (and (boundp (slot-role slot))
                             (get-domain (symbol-value (slot-role slot)))))
      collect (if (and (symbolp domain) (boundp domain)) (symbol-value domain) domain)))

;;
;; <symbol> Property t --> <resource> Property t
;;

(defmethod addTriple ((subject symbol) (predicate rdf:Property) object)
  (let ((domain nil))
    (cond ((object? subject)
           (addTriple (symbol-value subject) predicate object))
          ((setq domain (get-domain predicate))
           (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
             subject (name predicate) object subject (name domain))
           (addTriple subject rdf:type domain)
           (addTriple (symbol-value subject) predicate object))
          ((typep object rdfs:Resource)
           (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type rdfs:Resource."
             subject (name predicate) object subject)
           (addTriple subject rdf:type |rdfs:Resource|)
           (addTriple (symbol-value subject) predicate object))
          ((error "NOT YET")))))

(defmethod addTriple ((subject symbol) (predicate rdfs:Resource) object)
  (addTriple subject (change-class predicate rdfs:Resource) object))

;;
;; Property rdfs:range symbol --> Property rdfs:range <resource>
;;

(defmethod addTriple ((subject rdf:Property) (predicate (eql rdfs:range)) (object symbol))
  (case object
    ((datatype? object)
     (addInstance (class-of subject) (name subject) `((,(name predicate) ,object))))
    (otherwise 
     (cond ((object? object)
            (addTriple subject predicate (symbol-value object)))
           (t (let ((range (or (get-range predicate) |rdfs:Resource|)))
                (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
                  subject (name predicate) object object (name range))
                (addTriple object rdf:type range)
                (addTriple subject predicate (symbol-value object))))))))

;;;
;;;; <symbol> Property data  -->  <resource> Property data
;;;

(defmethod addTriple ((subject symbol) (predicate rdf:Property) (object cl:number))
  (let ((domain nil)
        (range nil))
    (cond ((object? subject)
           (addTriple (symbol-value subject) predicate object))
          ((setq domain (get-domain predicate))
           (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
             subject (name predicate) object
             subject (name domain))
           (addTriple subject rdf:type domain)
           (addTriple (symbol-value subject) predicate object))
          ((setq range (get-range predicate))
           (unless (typep object range)
             (error "Range violation:~S for ~S" object range))
           (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type rdfs:Resource"
             subject (name predicate) object subject)
           (addTriple subject rdf:type |rdfs:Resource|)
           (addTriple (symbol-value subject) predicate object))
          (t (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type rdfs:Resource"
               subject (name predicate) object subject)
             (addTriple subject rdf:type |rdfs:Resource|)
             (addTriple (symbol-value subject) predicate object)))))

(defmethod addTriple ((subject symbol) (predicate rdf:Property) (object cl:string))
  (let ((domain nil))
    (cond ((object? subject)
           (addTriple (symbol-value subject) predicate object))
          ((setq domain (get-domain predicate))
           (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
             subject (name predicate) object subject (name domain))
           (addTriple subject rdf:type domain)
           (addTriple (symbol-value subject) predicate object))
          (t ;; very new input without any information of property
           (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type rdfs:Resource"
             subject (name predicate) object subject)
           (addTriple subject rdf:type |rdfs:Resource|)
           (addTriple (symbol-value subject) predicate object)))))

;;;
;;;; <symbol> Property <symbol>  -->  <resource> Property <resource>
;;;
;;; Range constraint is used for satisfiability checking and proactive entailment.
;;; See entaiment rule rdfs3.

(defmethod addTriple ((subject symbol) (predicate rdf:Property) (object symbol))
  (cond ((subproperty-p predicate rdf:type)     ; accepts every subproperty of rdf:type but not rdf:type
         (unless (object? object)
           (let ((range (get-range predicate)))
             (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
               subject (name predicate) object object (name range))
             (addTriple object rdf:type range)))
         (addTriple subject rdf:type (symbol-value object))
         (addTriple subject predicate (symbol-value object)))
        ((object? subject)
         (addTriple (symbol-value subject) predicate object))
        ((object? object)
         (addTriple subject predicate (symbol-value object)))
        (t (let ((domain nil)
                 (range nil))
             (cond ((setq domain (get-domain predicate))
                    (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
                      subject (name predicate) object subject (name domain))
                    (addTriple subject rdf:type domain)
                    (addTriple (symbol-value subject) predicate object))
                   ((setq range (get-range predicate))
                    (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
                      subject (name predicate) object object (name range))
                    (addTriple object rdf:type range)
                    (addTriple subject predicate (symbol-value object)))
                   (t (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type rdfs:Resource." 
                        subject (name predicate) object object)
                      (addTriple object rdf:type |rdfs:Resource|)
                      (addTriple subject predicate (symbol-value object))))))))

;;;
;;;; <URI> Property t  -->  <symbol> Property t
;;;

(defmethod addTriple ((subject net.uri:uri) (predicate rdf:Property) object)
  (let ((symbol (uri2symbol subject)))
    (unless (symbolp symbol) (return-from addTriple))
    (prog1 (addTriple symbol predicate object)
      (setf (slot-value (symbol-value symbol) 'rdf:about) subject))))

;;;
;;;; <URI> symbol t  -->  <URI> Property t
;;;

(defmethod addTriple ((subject net.uri:uri) (predicate symbol) object)
  (unless (property? predicate)
    (export predicate (symbol-package predicate))
    (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type rdf:Property." 
      subject predicate object predicate)
    (addTriple predicate rdf:type rdf:Property))
  (addTriple subject (symbol-value predicate) object))

;;;
;;;; (quote <resource>) Property t --> <resource> Property t
;;;

(defmethod addTriple ((subject cons) (predicate rdf:Property) object)
  (if (eq (car subject) 'quote)
      (if (null (cddr subject))
          (addTriple (second subject) predicate object)
        (error "Cant happen!"))
    (error "Cant happen!")))

;;;
;;;; t <symbol> t  -->  t <Property> t
;;;
;;; If <symbol> is undefined, entailment rule rdf1 is applied.

(defmethod addTriple (subject (predicate symbol) object)
  (unless (property? predicate)
    (export predicate (symbol-package predicate))
    (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type rdf:Property." 
      subject predicate object predicate)
    (addTriple predicate rdf:type rdf:Property))
  (addTriple subject (symbol-value predicate) object))

;;;
;;;; t <URI> t  -->  t <symbol> t
;;;

(defmethod addTriple (subject (predicate net.uri:uri) object)
  (let ((symbol (uri2symbol predicate)))
    (unless (symbolp symbol) (return-from addTriple))
    (prog1 (addTriple subject symbol object)
      (setf (slot-value (symbol-value symbol) 'rdf:about) predicate))))

;;
;; Invalid Statements
;;

(defmethod addTriple ((subject symbol) (predicate (eql rdf:type)) (object rdfs:Resource))
  (error "Invalid statement: ~S ~S ~S." subject predicate object))

(defmethod addTriple ((subject (eql 'xsd:string)) predicate object)
  (error "Invalid statement: ~S ~S ~S." subject predicate object))
(defmethod addTriple ((subject (eql 'xsd:decimal)) predicate object)
  (error "Invalid statement: ~S ~S ~S." subject predicate object))
(defmethod addTriple ((subject (eql 'xsd:float)) predicate object)
  (error "Invalid statement: ~S ~S ~S." subject predicate object))
(defmethod addTriple ((subject (eql 'xsd:double)) predicate object)
  (error "Invalid statement: ~S ~S ~S." subject predicate object))
(defmethod addTriple ((subject (eql 'xsd:int)) predicate object)
  (error "Invalid statement: ~S ~S ~S." subject predicate object))
(defmethod addTriple ((subject (eql 'xsd:integer)) predicate object)
  (error "Invalid statement: ~S ~S ~S." subject predicate object))
(defmethod addTriple ((subject (eql 'xsd:long)) predicate object)
  (error "Invalid statement: ~S ~S ~S." subject predicate object))
(defmethod addTriple ((subject (eql 'xsd:short)) predicate object)
  (error "Invalid statement: ~S ~S ~S." subject predicate object))
(defmethod addTriple ((subject (eql 'xsd:positiveInteger)) predicate object)
  (error "Invalid statement: ~S ~S ~S." subject predicate object))
(defmethod addTriple ((subject (eql 'xsd:nonPositiveInteger)) predicate object)
  (error "Invalid statement: ~S ~S ~S." subject predicate object))
(defmethod addTriple ((subject (eql 'xsd:negativeInteger)) predicate object)
  (error "Invalid statement: ~S ~S ~S." subject predicate object))
(defmethod addTriple ((subject (eql 'xsd:nonNegativeInteger)) predicate object)
  (error "Invalid statement: ~S ~S ~S." subject predicate object))

;;
;; File Interface
;;

(defun intern-from-nodeID (ID)
  (assert (char= #\_ (elt ID 0)))
  (assert (char= #\: (elt ID 1)))
  (let ((package (or (find-package :_) (make-package :_ :use nil))))  ; by smh
    (let ((symbol (intern (subseq ID 2) package)))
      (export symbol package)
      symbol)))

(defun intern-langString (literal)
  "langString ::= '\"' string '\"' ( '@' language )?"
  (let ((i 0))
    (let ((string (subseq literal i (setq i (1+ (position-with-escape #\" literal (1+ i)))))))  ; " to "
      (setq string (subseq string 1 (1- (length string)))) ; strip " and "
      (cond ((and (< i (length literal)) (char= (char literal i) #\@))
             (let ((lang (parse-language literal (1+ i))))
               (make-instance 'rdf:inLang :lang (intern lang "keyword") :content string)))
            (t string)))))

(defun intern-datatypeString (literal)
  "datatypeString ::= langString '^^' uriref "
  (let ((i 0))
    (let ((string (subseq literal i (setq i (1+ (position-with-escape #\" literal (1+ i)))))))  ; " to "
      (cond ((and (< i (1- (length literal))) (char= (char literal i) #\^) (char= (char literal (1+ i)) #\^))
             (let ((uriref (parse-uriref literal (1+ (1+ i)))))
               (make-instance (uri2symbol (subseq uriref 1 (1- (length uriref))))
                 :value (intern-langString string))))
            (t (intern-langString string))))))

(defun intern-literal (literal)
  (cond ((datatypeString-p literal 0) (intern-datatypeString literal))
        ((langString-p literal 0) (intern-langString literal))
        ((error "Illegal literal."))))

(defun intern-from-QName (QName)
  (multiple-value-bind (name space) (name&space QName)
    (let ((package (if space (find-package space) *package*)))
      (assert (or package
                  (and (y-or-n-p "Make package ~A?" space)
                       (setq package (make-package space :use nil)))))  ; by smh
      (let ((symbol (intern name package)))
        (export symbol package)
        symbol))))

(defun addTriple-from-file (subject predicate object)
  (cond ((and (null subject) (null predicate) (null object))) ; Null line
        ((and (null predicate) (null object)))                ; Comment line
        (t (addTriple (intern-subject subject) (intern-predicate predicate) (intern-object object)))))

(defun intern-subject (str)
  (cond ((uriref-p str 0) (uri2symbol (subseq str 1 (1- (length str)))))
        ((ID-p str 0) (intern-from-nodeID str))
        ((QName-p str 0)  (intern-from-QName str))
        ((error "Illegal subject:~%~A" str))))

(defun intern-predicate (str)
  (cond ((uriref-p str 0) (uri2symbol (subseq str 1 (1- (length str)))))
        ((QName-p str 0)  (intern-from-QName str))
        ((error "Illegal predicate:~%~A" str))))

(defun intern-object (str)
  (cond ((uriref-p str 0) (uri2symbol (subseq str 1 (1- (length str)))))
        ((ID-p str 0) (intern-from-nodeID str))
        ((literal-p str 0)(intern-literal str))
        ((QName-p str 0)  (intern-from-QName str))
        ((error "Illegal object:~~%~A" str))))

#|

(/. xsd:integer rdfs:subClassOf xsd:string)  -> ERROR
(/. xsd:integer rdfs:subClassOf xsd:decimal) -> ERROR

(/. prop1 rdfs:range xsd:string)
(/. foo prop1 25)                     -> ERROR

(/. prop2 rdfs:range xsd:integer)
(/. foo prop2 "25")                   -> 25

(/. prop3 rdf:type rdf:Property)
(/. bar rdfs:subClassOf prop3)         -> ERROR

(/. bar rdf:type rdf:Property)
(/. bas rdfs:subPropertyOf bar)
(/. bar rdfs:domain Domain1)
(/. bas rdfs:domain Domain2)
(/. bar rdfs:range Range1)
(/. bas rdfs:range Range2)
(/. baz1 bas baz2)

(defun revert-slot (slotd)
  (let ((role (mop:slot-definition-name slotd))
        (filler (mop:slot-definition-initform slotd))
        (readers (mop:slot-definition-readers slotd))
        (writers (mop:slot-definition-writers slotd)))
    `(:name ,role :initargs (,role) :initform ,filler :type ,filler
            :readers ,readers :writers ,writers)))

(defun revert-slots (dslots)
  (loop for slotd in dslots collect (revert-slot slotd)))

|#

(defun get-triple (resource)
  (when (null resource) (return-from get-triple))
  (assert (typep resource rdfs:Resource))
  (flet ((name-in-get-triple (rsc)
           (cond ((rsc-object-p rsc) (name rsc))
                 ((and (symbolp rsc) (nodeID? rsc)) rsc)
                 ((error "Cant happen!"))))
         (object-in-get-triple (rsc)
           (cond ((rsc-object-p rsc) rsc)
                 ((and (symbolp rsc) (nodeID? rsc)) (symbol-value rsc))
                 ((error "Cant happen!")))))
    (let ((subject (name-in-get-triple resource)))
      (append 
       (mapcar #'(lambda (ty) `(,subject rdf:type ,(name-in-get-triple ty)))
         (mklist (mclasses (object-in-get-triple resource))))
       (loop for slot in (get-slots resource)
           append (let ((role (slot-role slot))
                        (forms (slot-forms slot)))
                    (mappend #'(lambda (filler)
                                 (cond ((typep filler rdfs:Literal)
                                        `((,subject ,role ,filler)))
                                       ((rsc-object-p filler)
                                        (cond ((named-p filler)
                                               `((,subject ,role ,(name filler))))
                                              (t (let ((nodeid (make-unique-nodeID "gx")))
                                                   (setf (symbol-value nodeid) filler)
                                                   (cons
                                                    `(,subject ,role ,nodeid)
                                                    (get-triple nodeid))))))
                                       (t `((,subject ,role ,filler)))))
                             forms)))))))

;; End of module
;; --------------------------------------------------------------------

