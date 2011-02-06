;;;; -*- Mode: Lisp -*-

;;;; a.lisp --
;;;; Replacing MAKE-INSTANCE and other DEFSTRUCT constructors while
;;;; adding a few niceties with compound type specifiers.
;;;;
;;;; Please see file COPYING for copyright and licensing information.

(defpackage :x
  (:use :common-lisp)
  (:export :new :an :a :type-cons :register-structure-definer
           :numeric-real-types :compute-data-dimensions))


(in-package :x)

;;;;===========================================================================
;;;; Prelude

;;; We need to check if an implementation has certain classes or not,
;;; e.g., FIXNUM.  If that is the case we stash :CLASS-<class-name>-EXISTS
;;; in *FEATURES*, so that we can properly conditionalized the code.
;;; Just skip this code on a cursory reading.  It is not that important.

(eval-when (:load-toplevel :compile-toplevel :execute)
  (flet ((check-implementation-class (class-name)
           (when (find-class class-name nil)
             (pushnew (intern (format nil "CLASS-~A-EXISTS" class-name) (find-package :keyword))
                      *features*))))
    (check-implementation-class 'fixnum)
    (check-implementation-class 'short-float)
    (check-implementation-class 'single-float)
    (check-implementation-class 'double-float)
    ;; Other class/type names here.
    ))

;;;;---------------------------------------------------------------------------
;;;; NEW operator Protocol.
;;;; The current implementation is based on generic functions.  An
;;;; alternative would be to define a "constructor" function and have NEW
;;;; as a macro expanding in the proper constructor. The current
;;;; implementation is done to introduce the concept and the syntax.
;;;; Efficiency may be achieved by means of compiler macros: any taker?

(defgeneric new (what &rest arguments)
  (:documentation "Generic 'construction' function.
The NEW generic function is a single entry point for MAKE-INSTANCE,
DEFSTRUCT and other constructors, e.g., for HASH-TABLEs and ARRAYs.
Methods for NEW can be defined specialized for new classes.
Structure classes need to register their constuctor; see
REGISTER-STRUCTURE-CONSTRUCTOR.  Compound type specifiers must be
specialized by defining methods for TYPE-CONS."))

;;;; NEWQ macro. (deprecated)
(defmacro newq (what &rest arguments)
  "The NEWQ macro is a convenience syntactic shortcut to the NEW function.
The macro expands into a call to NEW without evaluating the first argument."
  `(new ',what ,@arguments))

(defmacro a (what &rest arguments)
  "The A macro is a convenience syntactic shortcut to the NEW function."
  `(new ',what ,@arguments))

(defmacro an (what &rest arguments)
  "The AN macro is a convenience syntactic shortcut to the NEW function."
  `(new ',what ,@arguments))


(defgeneric type-cons (type-name spec arguments)
  (:documentation "The TYPE-CONS Generic Function.
TYPE-CONS is the entry point to specialize in order to handle compound
type specifiers.  The only assumption is that a compound type
specifier is a list with a symbol as first element, as described in
Section 4.2.3 'Type Specifiers' of the ANSI CL Specification.
Each method dispatches on the TYPE-NAME, which is a symbol.  SPEC is
the CDR of the full type specifier and ARGUMENTS is the list of
arguments passed to NEW.
TYPE-CONS is meant to be spcialized but not called directly.

For example, suppose you have:

   (deftype matrix (n m &optional (base-type 'double-float))
      `(array ,base-type (,n ,m)))

You may want to define something along these lines.

   (defmethod type-cons ((what (eql 'matrix)) spec arguments)
      (destructuring-bind (n m &optional (base-type 'double-float))
          spec
        (assert (and (<= 0 n (1- array-dimension-limit))
                     (<= 0 m (1- array-dimension-limit))))
        (apply #'make-array (list n m)
               :element-type base-type
               arguments)))"))


;;;;---------------------------------------------------------------------------
;;;; Implementation.

(defmacro type-check (place type)
  "Checks that PLACE contains an object of type TYPE.
It works like CHECK-TYPE, but TYPE is evaluated. 
It also returns T if the check succeeds."
  `(progn
     (assert (typep ,place ,type) (,place)
       'type-error
       :expected-type ,type
       :datum ,place)
     t))


(defmacro check-optional-args-number (arglist min max &optional after)
  "Utility macro producing ASSERTs that check the number of arguments."
  (if (equal min max)
      `(assert (= ,min (list-length ,arglist))
           (,arglist)
         "Wanted exactly ~D argument~@[ after ~S~]; got ~D."
         ,min ,after (list-length ,arglist))
      `(assert (<= ,min (list-length ,arglist) ,max)
           (,arglist)
         "Wanted at least ~D and at most ~D arguments~@[ after ~S~]; got ~D."
         ,min ,max ,after (list-length ,arglist))))


(defmethod new ((what symbol) &rest arguments)
  "SYMBOL Method. Dispatches on the class named by the argument."
  (apply #'new (find-class what) arguments))


(defmethod new ((what standard-class) &rest arguments)
  "STANDARD-CLASS Method. Applies MAKE-INSTANCE to the class and ARGUMENTS.  This method is, to
all effects, MAKE-INSTANCE in disguise."
  (apply #'make-instance what arguments))


(defmethod new ((what structure-class) &rest arguments)
  "STRUCTURE-CLASS Method. Applies the constructor of the STRUCTURE-CLASS to the ARGUMENTS.  The
constructor must have been registered with
REGISTER-STRUCTURE-CONSTRUCTOR to be applicable."
  (apply (find-structure-constructor what) arguments))


(defmethod new ((what built-in-class) &rest arguments)
  "BUILT-IN-CLASS Method. Signals an 'undefined' error.  This method is a sink for the
built-in classes which do not have a specialized method built with EQL."
  (declare (ignore arguments))
  (error "Undefined NEW operator for built-in class ~A." (class-name what)))


(defmethod new :around ((what class) &rest arguments)
  "CLASS Around Method. This method helps dealing with some of the vagaries of the CL type
system, e.g., COMPLEX and RATIONAL types with their relationship."
  (declare (ignore arguments))
  (let ((r (call-next-method))
        (cn (class-name what)))
    ;; Hairy code due to CL type system.
    ;; Each clause before the last is a "special case".
    (cond ((eq cn 'complex)
           (assert (typep r '(or complex rational))))
          (t
           (type-check r cn)))
    r))


;;; Specialized constructors.

(defmethod new ((what (eql (find-class 'array))) &rest arguments)
  "ARRAY Method. Calls MAKE-ARRAY after having manipulated the ARGUMENTS.
The 'bare' constructor for objects of 'type' ARRAY  actually behaves
as having the following simple syntax:

   new 'array &optional <initial-contents> &rest <MAKE-ARRAY keywords>

I.e., the constructor figures out the dimensions of the
<initial-contents> before dispatching to MAKE-ARRAY."
  (let ((initial-contents (first arguments)))
    (apply #'make-array (compute-data-dimensions initial-contents nil t)
               :initial-contents initial-contents
               (rest arguments))))


(defmethod new ((what (eql (find-class 'vector))) &rest arguments)
  "VECTOR Method. Applies the function VECTOR to the ARGUMENTS."
  (apply #'vector arguments))


(defmethod new ((what (eql (find-class 'string))) &rest arguments)
  "STRING Method. Applies the function STRING to the first of ARGUMENTS."
  (assert (= (list-length arguments) 1))
  (string (first arguments)))


(defmethod new ((what (eql (find-class 'list))) &rest arguments)
  "LIST Method. Applies the function VECTOR to the ARGUMENTS."
  (apply #'list arguments))


(defmethod new ((what (eql (find-class 'character))) &rest arguments)
  "CHARACTER Method. Applies the function CHARACTER to the first of ARGUMENTS."
  (assert (= (list-length arguments) 1))
  (character (first arguments)))


(defmethod new ((what (eql (find-class 'symbol))) &rest arguments)
  "SYMBOL Method. The SYMBOL method has the following syntax.

   NEW 'SYMBOL S &optional (PACKAGE *PACKAGE*) COPY-PROPERTIES

S can be a STRING or a SYMBOL.  If it is a SYMBOL, S is first copied,
using COPY-SYMBOL and COPY-PROPERTIES (cfr., COPY-SYMBOL in the CL
Standard).  If S is a STRING then MAKE-SYMBOL is initially called.  If
PACKAGE is NIL then a fresh, uninterned symbol is returned, otherwise
the resulting symbol is IMPORTed in PACKAGE."

  ;; Note: it is a desing decision *not* to conflate GENSYM/GENTEMP in
  ;; this method.

  (assert (<= 1 (list-length arguments) 3) (arguments)
    "Wanted at least 1 arguments and at most 3 after ~S." what)
  (destructuring-bind (s &optional (pkg *package*) copy-props)
      arguments
    (etypecase s
      (string (if pkg
                  (intern s pkg)
                  (make-symbol s)))
      (symbol
       (let ((ns (copy-symbol s copy-props)))
         (if pkg
             (progn
	       (import ns pkg)
	       (find-symbol (symbol-name ns) pkg))
             ns))))))


(defmethod new ((what (eql (find-class 'pathname))) &rest arguments)
  "PATHNAME Method. The PATHNAME method dispatches on the first element of ARGUMENTS.  If
it is a KEYWORD, it assumes that MAKE-PATHNAME is to be called,
applied to all the ARGUMENTS.
Otherwise it checks whether the first of ARGUMENTS ia a STRING, a
STREAM or a PATHNAME, in which case is passes it to the function
PATHNAME."
  (if arguments
      (etypecase (first arguments)
        ((or string stream pathname) (pathname (first arguments)))
        (keyword (apply #'make-pathname arguments)))
      (make-pathname)))

;;; Numbers...

(defmethod new ((what (eql (find-class 'number))) &rest arguments)
  "NUMBER Method. The NUMBER method just checks that the argument is a number and
returns it."
  (assert (= (list-length arguments) 1))
  (let ((n (first arguments)))
    (check-type n number)
    n))


(defmethod new ((what (eql (find-class 'complex))) &rest arguments)
  "COMPLEX Method. The COMPLEX method applies the function COMPLEX to the arguments."
  
  ;; It could be useful to have the following syntax
  ;;
  ;;    (new 'complex r &optional i (c-plane-type 'real))
  ;;
  (assert (<= 1 (list-length arguments) 2) (arguments)
    "Wanted 1 or 2 arguments after ~S." what)
  (apply #'complex arguments))


(defmethod new ((what (eql (find-class 'real))) &rest arguments)
  (check-optional-args-number arguments 1 1)
  (let ((n (first arguments)))
    (check-type n real)
    n))


(defmethod new ((what (eql (find-class 'float))) &rest arguments)
  (check-optional-args-number arguments 1 1)
  (let ((n (first arguments)))
    (check-type n float)
    n))


(defmethod new ((what (eql (find-class 'rational))) &rest arguments)
  (check-optional-args-number arguments 1 2)
  (let ((n (first arguments))
        (d (or (second arguments) 1)))
    (check-type n rational)
    (check-type d rational)
    (/ n d)))


(defmethod new ((what (eql (find-class 'ratio))) &rest arguments)
  (check-optional-args-number arguments 2 2)
  (let ((n (first arguments))
        (d (second arguments)))
    (check-type n integer)
    (check-type d (integer 2))
    (/ n d)))


(defmethod new ((what (eql (find-class 'integer))) &rest arguments)
  (check-optional-args-number arguments 1 1)
  (let ((n (first arguments)))
    (check-type n integer)
    n))

;;; Conditionalized methods.  Not all implementations admit the
;;; following as classes.

#+class-fixnum-exists
(defmethod new ((what (eql (find-class 'fixnum))) &rest arguments)
  (check-optional-args-number arguments 1 1)
  (let ((n (first arguments)))
    (check-type n fixnum)
    n))

#+class-short-float-exists
(defmethod new ((what (eql (find-class 'short-float))) &rest arguments)
  (check-optional-args-number arguments 1 1)
  (let ((n (first arguments)))
    (check-type n short-float)
    n))

#+class-single-float-exists
(defmethod new ((what (eql (find-class 'single-float))) &rest arguments)
  (check-optional-args-number arguments 1 1)
  (let ((n (first arguments)))
    (check-type n single-float)
    n))

#+class-double-float-exists
(defmethod new ((what (eql (find-class 'double-float))) &rest arguments)
  (check-optional-args-number arguments 1 1)
  (let ((n (first arguments)))
    (check-type n double-float)
    n))
  

;;;; NB.  Now the following works in a useful way.
;;;;
;;;;     (newq integer (read))
;;;;


;;; Hash Tables.

(defun copy-hash-table (ht)
  "Copies and HASH-TABLE. This function is implementation dependent.  The basic behavior
provided just copies an hash-table with the standard components."
  (declare (type hash-table ht))
  (let ((test (hash-table-test ht))
        (size (hash-table-size ht))
        (rehash-size (hash-table-rehash-size ht))
        (rehash-threshold (hash-table-rehash-threshold ht)))
    (let ((new-ht (make-hash-table :test test :size size
                            :rehash-size rehash-size :rehash-threshold rehash-threshold)))
      (maphash (lambda (k v)
                 (setf (gethash k new-ht) v))
               ht)
      new-ht)))
        

(defmethod new ((what (eql (find-class 'hash-table)))
                &rest arguments)
  "HASH-TABLE Method. The HASH-TABLE method admits a non-traditional syntax.  It can be
invoked in two forms.

   new 'HASH-TABLE
        &optional <pairs>
        &rest <hash-table-standard-args>
        &key &allow-other-keys
or

   new 'HASH-TABLE
        &rest <hash-table-standard-args>
        &key &allow-other-keys

I.e., it dispatches on the first of the ARGUMENTS to decide whether it
also has to 'fill' the newly created hash table; in this last case, it
behaves as a 'copy' constructor.  If the first of the ARGUMENTS is a
hash-table, the keywords are ignored.  If it is a keyword then it is
assumed that the call must resolve to a simple call to
MAKE-HASH-TABLE.  Otherwise, <pairs> must be an A-LIST of pairs

  (<key> . <value>)

which are then fed into the newly created hash-table.

The result is an hash table."
  (let ((a1 (first arguments)))
    (etypecase a1
      (hash-table
       (copy-hash-table a1))

      (cons
       ;; An A-LIST.
       (loop with new-ht = (apply #'make-hash-table (rest arguments))
             for (k . v) in a1
             do (setf (gethash k new-ht) v)
             finally (return new-ht)))

      ((or keyword null)
       (apply #'make-hash-table arguments)))))

#+old-version
(defmethod new ((what (eql 'hash-set))
                &rest arguments)
  "HASH-SET Method.

A specialised method yielding a HASH-TABLE with BOOLEAN values.
"
  (let* ((contents-p (consp (first arguments)))
         (ht (apply #'make-hash-table
                    (if contents-p (rest arguments) arguments)))
         )
    (when contents-p
      (loop for e in (first arguments)
            do (setf (gethash e ht) t)))
    ht))


(defmethod new ((what (eql 'hash-set))
                &rest arguments)
  "HASH-SET Method. A specialised method yielding a HASH-TABLE with BOOLEAN values.  It
can be invoked in two forms.

   new 'HASH-SET
        &optional <sequence-set>
        &rest <hash-table-standard-args>
        &key &allow-other-keys
or

   new 'HASH-SET
        &rest <hash-table-standard-args>
        &key &allow-other-keys

I.e., it dispatches on the first of the ARGUMENTS to decide whether it
also has to 'fill' the newly created hash table; in this last case, it
behaves as a 'copy' constructor for the hash-table skeleton, and then
it substitutes the values or the resulting hash-table with T.  If the
first of the ARGUMENTS is a hash-table, the keywords are ignored.  If
it is a keyword then it is assumed that the call must resolve to a
simple call to MAKE-HASH-TABLE.  Otherwise, <sequence-set> must be an
SEQUENCE of elements of the set.

The result is an hash table."
  (let ((a1 (first arguments)))
    (etypecase a1
      (hash-table
       (let ((hs (copy-hash-table a1)))
         (maphash (lambda (k v)
                    (declare (ignore v))
                    (setf (gethash k hs) t))
                  a1)
         hs))

      ((or keyword null)
       (apply #'make-hash-table arguments))

      (sequence
       (loop with new-ht = (apply #'make-hash-table (rest arguments))
             for i from 0 below (length a1)
             for k = (elt a1 i) ; A little inefficient.
             do (setf (gethash k new-ht) t)
             finally (return new-ht)))
      )))


;;;; Compound (hairy) type specifiers.

(defmethod new ((what cons) &rest arguments)
  "CONS Method. This is the method that handles the compound type specifiers.  It
eventually calls TYPE-CONS, which is in charge of dealing with each
compound type specifier. In order to inform the NEW operator of a new compound type specifier
it is thus sufficient to define an appropriate TYPE-CONS method."
  (type-cons (first what) (rest what) arguments))

#| This is a little paranoid.
(defmethod new :around ((what cons) &rest arguments)
  (let ((r (call-next-method)))
    (assert (typep r what))
    r))
|#


;;;;---------------------------------------------------------------------------
;;;; Types without classes.

;;; Number type classes.

(defmethod new ((what (eql 'signed-byte)) &rest arguments)
  (type-cons 'signed-byte () arguments))


(defmethod new ((what (eql 'unsigned-byte)) &rest arguments)
  (type-cons 'unsigned-byte () arguments))


(defmethod new ((what (eql 'bit)) &rest arguments)
  (apply #'new '(integer 0 1) arguments))


;;;;---------------------------------------------------------------------------
;;;; Structure hacks.
;;;;
;;;; There is no portable way to fetch a "constructor" for a
;;;; structure.  Different implementations must have an internal
;;;; function to deal with #S(...) but this is not generally available.
;;;;
;;;; The kludge here is to provide a modicum of infrastructure to
;;;; 'record' constructors.

(defvar *structure-constructors* (make-hash-table :test #'eq))

(defun register-structure-constructor (sc constructor)
  "Registers CONSTRUCTOR for structure class SC.

The CONSTRUCTOR will then be available to NEW whenever a SC needs to be
created."
  (etypecase sc
    (symbol (setf (gethash sc *structure-constructors*) constructor))
    (structure-class
     (register-structure-constructor (class-name sc) constructor))))


(defun find-structure-constructor (sc &optional errorp)
  (declare (type structure-class sc))
  (let* ((sc-name (class-name sc))
         (constructor-name
          (find-symbol (format nil "MAKE-~A" sc-name)
                       (symbol-package sc-name)))
         (struct-cons (gethash sc-name
                               *structure-constructors*
                               (and (fboundp constructor-name)
                                    (symbol-function constructor-name))))         )
    (cond (struct-cons)
          (errorp (error "Cannot find a constructor for structure class ~S."
                         sc-name)))))
        

;;;;---------------------------------------------------------------------------
;;;; TYPE-CONS

(defmethod type-cons ((what (eql 'vector)) spec arguments)
  (destructuring-bind (&optional
                       (element-type t)
                       (size (length arguments))
                       &rest
                       vector-features)
      spec
    (apply #'make-array size
           :element-type element-type
           :initial-contents arguments
           vector-features)))


#+old-version
(defmethod type-cons ((what (eql 'array)) spec arguments)
  (let ((initial-contents (first arguments)))
    (destructuring-bind (&optional
                         (element-type t)
                         (dimension-or-rank (list (length initial-contents)))
                         &rest array-features)
        spec
      (let ((dims (if (and (integerp dimension-or-rank)
                           (<= 0 dimension-or-rank (1- array-rank-limit)))
                      (compute-dimensions dimension-or-rank initial-contents)
                      dimension-or-rank))
            (array-init (if initial-contents
                            (append array-features `(:initial-contents ,initial-contents))
                            array-features))

            )
        (apply #'make-array dims
               :element-type element-type
               array-init)))))


;;; TYPE-CONS ARRAY new version.

(defmethod type-cons ((what (eql 'array)) spec arguments)
  "ARRAY Method. It constructs an array after having manipulated SPEC and ARGUMENTS;
the overall effect is similar to the NEW ARRAY method.
The TYPE-CONS method allows NEW to use the following syntax:

   new '(array &optional (<element-type> T) <dimension-spec> &rest <MAKE-ARRAY-kwds-1>
               &key &allow-other-keys) 
          &optional <initial-contents> &rest <MAKE-ARRAY-kwds-2>  &key  &allow-other-keys

I.e., the constructor figures out the dimensions of the
<initial-contents> before dispatching to MAKE-ARRAY.  Further checks
are made to ensure that the shape of <initial-contents> (if supplied)
is acceptable. MAKE-ARRAY-kwds-1 and MAKE-ARRAY-kwds-2 are the normal MAKE-ARRAY
parameters.  The actual call to MAKE-ARRAY is eventually of the form:

   (apply 'make-array <computed-dimensions>
          (append (and <initial-contents> (list :initial-contents <initial-contents>))
             MAKE-ARRAY-kwd-1 MAKE-ARRAY-kwd-2))

Therefore, order among the MAKE-ARRAY keywords is important.
It should be noted that the followingis a valid, yet constrained, type specifier.

   (array &optional (<element-type> T) <dimension-spec>)"

  (let ((initial-contents (first arguments))
        (other-array-features (rest arguments)))
    (destructuring-bind (&optional
                         (element-type t)
                         (dimension-or-rank
                          (compute-data-dimensions initial-contents
                                                   nil
                                                   t))
                         &rest array-features)
        spec
      (let ((dims (if (and (integerp dimension-or-rank)
                           (<= 0 dimension-or-rank (1- array-rank-limit)))
                      (compute-data-dimensions initial-contents
                                               dimension-or-rank
                                               t)
                      dimension-or-rank))
            (array-init-kwds (append (when initial-contents
                                       `(:initial-contents ,initial-contents))
                                     array-features
                                     other-array-features))
            )
        (apply #'make-array dims
               :element-type element-type
               array-init-kwds)
        ))))


(defun compute-dimensions (rank data)
  (labels ((compdim (r d)
             (unless (<= r 0)
               (unless (typep d 'sequence)
                 (error "Data does not have rank ~D; rank ~D data ~S is not a sequence."
                        rank
                        r
                        d))
               (unless (zerop (length d))
                 (cons (length d)
                       (compdim (1- r) (elt d 0)))))))
    (compdim rank data)))


;;; compute-data-dimensions --
;;; Version of COMPUTE-DIMENSIONS with better semantics and checks.
;;; This is just a hack.  I am sure it'd be easier to write an
;;; equivalent one with better space complexity.

(defun compute-data-dimensions (data
                                &optional
                                rank
                                (error-if-unequal-p t)
                                &aux (nested-sequences data))
  "Computes the dimensions of DATA.
DATA is a sequence of nested sequences.  RANK can be passed to limit
the depth of the reconstruction; if NIL (the default) the dimensions
are computed down to 'atoms'.  ERROR-IF-UNEQUAL-P controls whether an
error is signalled if the shape of DATA is 'ragged'. An error is also
signalled if RANK is higher than the actual DATA rank."
  (labels ((compute-nested-lengths (nested-sequences r)
             ;; Computes the lengths of the nested sequences.
             ;; Fore each (nested) sequence it returns a "summary
             ;; list" of the form:
             ;;
             ;;   (<seq-rank> . <nested-seq-summary>*)
             ;;
             (if (and r (<= r 0))
                 0
                 (etypecase nested-sequences
                   (null '(0))
                   (sequence
                    (cons (length nested-sequences)
                          (map 'list 
                               (lambda (ns)
                                 (compute-nested-lengths ns (and r (1- r))))
                               nested-sequences)))
                   (atom
                    (when (and r (plusp r))
                      (error "Rank of data is less than ~D." rank))
                    0))))

           (build-dims (nls)
             (cond ((consp nls)
                    (cons (first nls)
                          (build-dims (second nls))))
                   ((or (null nls)
                        (and (numberp nls)
                             (zerop nls)))
                    ())
                   (t
                    (error "Malformed data nested lengths.")))))
    (let* ((dims-tree (compute-nested-lengths nested-sequences rank))
           (d2 (second dims-tree))
           )
      (declare (dynamic-extent dims-tree d2))
      (when (and error-if-unequal-p
                 (notevery (lambda (d) (equal d d2)) (cddr dims-tree)))
        (error "Unequally shaped data; dimension shape tree ~S." dims-tree))
      (build-dims dims-tree))))


;;;;---------------------------------------------------------------------------
;;;; Number type constructors.

(deftype numeric-real-types ()
  '(member real
           float
           short-float
           single-float
           double-float
           long-float
           rational
           integer
           ratio
           fixnum
           bignum))


(declaim (inline check-numeric-interval-element))

(defun check-numeric-interval-element (n num-type-name l u)
  (type-check n `(,num-type-name ,l ,u))
  n)


#+old-version
(defun check-numeric-interval-element (n num-type-name l u)
  (let ((num-type `(,num-type-name ,l ,u)))
    ;; More checks here.
    (assert (typep n num-type) (n)
      'type-error
      :expected-type num-type
      :datum n
      )
    n))


(defmethod type-cons ((what (eql 'complex)) spec arguments)
  (check-optional-args-number arguments 1 2)
  (assert (= 1 (list-length spec)))
  (let ((ct (first spec))
        ;; (nc (apply #'new (find-class 'complex) arguments))
        (nc (apply #'complex arguments)) ; Shortcut.
        )
    (coerce nc `(complex ,ct)) ; This should fail if necessary.
    ))


(defgeneric interval-type-cons (num-type lower-bound upper-bound arguments))


(defmethod no-applicable-method ((itc (eql #'interval-type-cons)) &rest args)
  (error "Type ~A does not have a numeric interval constructor." (first args)))


(defmethod interval-type-cons ((num-type symbol) l u arguments)
  (check-numeric-interval-element (first arguments) num-type l u))


;;; interval-type-cons preconditions.

(defmethod interval-type-cons :before ((num-type symbol) l u arguments)
  (let ((nargs (list-length arguments)))
    (check-type num-type numeric-real-types)
    (check-type l (or number (cons number null) (eql *)))
    (check-type u (or number (cons number null) (eql *)))
    (if (member num-type '(ratio rational) :test #'eq)
        (assert (<= 1 nargs 2))
        (assert (= nargs 1))
        )))

;;; Specialized INTERVAL-TYPE-CONS methods.

;;; RATIONAL

(defun make-rational (n &optional (d 1))
  (check-type n rational)
  (check-type d rational)
  (/ n d))


(defmethod interval-type-cons ((num-type (eql 'rational)) l u arguments)
  (check-numeric-interval-element (apply #'make-rational arguments) 'rational l u))


;;; RATIO
;;; According to the CLHS there is no compound type specifier for RATIO


;;; FIXNUM

(defmethod interval-type-cons ((num-type (eql 'fixnum)) l u arguments)
  (let ((lb (if (consp l) (1+ (first l)) l))
        (ub (if (consp u) (1- (first u)) u))
        )
    (check-type lb (or fixnum (eql '*)))
    (check-type ub (or fixnum (eql '*)))

    (let ((l (if (eq '* l) most-negative-fixnum l))
          (u (if (eq '* l) most-positive-fixnum u))
          )
      (coerce (interval-type-cons 'integer l u arguments) 'fixnum))))


#|
(defmethod type-cons ((what (eql 'integer)) spec arguments)
  (if (null spec)
      (apply #'new (find-class 'integer) arguments)
      (destructuring-bind (l &optional (u '*))
          spec
        (interval-type-cons 'integer l u arguments))))
|#


(defmethod type-cons ((what symbol) spec arguments)
  (if (typep what 'numeric-real-types)
      (destructuring-bind (l &optional (u '*))
          spec
        (interval-type-cons what l u arguments))
      (error "No known type constructor for (~S~{ ~S~}) with arguments ~S."
             what
             spec
             arguments)))


(defmethod type-cons ((what (eql 'signed-byte)) spec arguments)
  (if (null spec)
      (apply #'new (find-class 'integer) arguments)
      (let ((nargs (list-length arguments)))
        (assert (= nargs 1))
        (assert (= 1 (length spec)))
        (let ((n (first arguments))
              (nbits (first spec))
              )
          (assert (plusp nbits) (spec)
            "Malformed SIGNED-BYTE type specifier; ~D is not a positive integer."
            nbits)
          (type-check n `(signed-byte ,@spec))
          n
          ))))


(defmethod type-cons ((what (eql 'unsigned-byte)) spec arguments)
  (if (null spec)
      (apply #'new '(integer 0 *) arguments)
      (let ((nargs (list-length arguments)))
        (assert (= nargs 1))
        (assert (= 1 (length spec)))
        (let ((n (first arguments))
              (nbits (first spec))
              )
          (assert (plusp nbits) (spec)
            "Malformed UNSIGNED-BYTE type specifier; ~D is not a positive integer."
            nbits)
          (type-check n `(unsigned-byte ,@spec))
          n
          ))))


(defmethod type-cons ((what (eql 'mod)) spec arguments)
  (assert (= 1 (list-length arguments)))
  (assert (= 1 (list-length spec)))
  (let ((n (first arguments)))
    (type-check (first arguments) `(mod ,@spec))
    n))


;;;; Hairy hash tables.

(defvar *default-dummy-hash-table* (make-hash-table))

(defmethod type-cons ((what (eql 'hash-table)) spec arguments)
  "The HASH-TABLE Method.

It constructs a HASH-TABLE by handling a 'type specifier' with a
syntax that makes the following call the NEW possible:

    (new '(hash-table &rest ht-keys
                      &key
                       (test #'eql)
                       (size (hash-table-size *default-dummy-hash-table*))
                       (rehash-size (hash-table-rehash-size *default-dummy-hash-table*))
                       (rehash-threshold (hash-table-rehash-threshold *default-dummy-hash-table*))
                       &allow-other-keys)
         &rest arguments)

The defaults for each of the keyword arguments are implementation dependent."
  (destructuring-bind (&rest
                       ht-keys
                       &key
                       (test #'eql)
                       (size (hash-table-size *default-dummy-hash-table*))
                       (rehash-size (hash-table-rehash-size *default-dummy-hash-table*))
                       (rehash-threshold (hash-table-rehash-threshold *default-dummy-hash-table*))
                       &allow-other-keys
                       )
      spec
    (declare (ignore size rehash-size rehash-threshold))

    ;; Fixing a possible snag.
    (when (and (consp test)
               (or (eq (first test) 'quote)
                   (eq (first test) 'function)))
      (setf (getf ht-keys :test) (symbol-function (second test))))

    (loop with ht = (apply #'make-hash-table ht-keys)
          for (k . v) in (first arguments)
          do (setf (gethash k ht) v)
          finally (return ht))
    ))


;;;; end of file -- a.lisp --
