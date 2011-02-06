;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
;;;
;;;  Based on defpackage-form.lisp by Michael Weber <michaelw@foldr.org>, 2008
;;;
;;;; Issues:
;;;
;;; * lose with reader conditionals in DEFPACKAGE form
;;;
;;; * default :USE packages are implementation dependent
;;;
;;; Notes
;;;
;;; * clhs DEFPACKAGE:
;;;
;;;   "The collection of symbol-name arguments given to the
;;;  options :shadow, :intern, :import-from, and :shadowing-import-from
;;;  must all be disjoint; additionally, the symbol-name arguments given
;;;  to :export and :intern must be disjoint. Disjoint in this context is
;;;  defined as no two of the symbol-names being string= with each
;;;  other. If either condition is violated, an error of type
;;;  program-error should be signaled. ;"
;;;
;;; "The :intern option is useful if an :import-from or
;;;  a :shadowing-import-from option in a subsequent call to
;;;  defpackage (for some other package) expects to find these symbols
;;;  accessible but not necessarily external."
;;;
;;;;

(defpackage :x
  (:use #:common-lisp)
  (:export #:with-package #:defpackage-form #:filter-exported-symbols-iterator
           #:filter/identity #:apply-filter #:package-designator #:package-documentation
           :package-list :package-count :pprint-package-table :package-api-list
           :package-inquiry-table #:symbol-designator #:symbol-status
    :package-info
    #:flatten-to-cells
    #:ensure-package
    #:sort-symbols
    #:symbol-importedp
    #:symbol-exportedp
    #:symbol-presentp
    #:symbol-accessiblep
    #:pprint-defpackage
    #:package-export-list
    #:all-package-symbols
    #:package-symbol-count))

;;;; Code
(in-package :x)

(defun flatten-to-cells (tree)
  (let (list)
    (labels ((walk (subtree)
               (when (and tree (consp subtree))
                 (push subtree list)
                 (walk (car subtree))
                 (walk (cdr subtree)))))
      (walk tree))
    (reverse list)))

(defmacro with-package (package &body body)
  (flet ((local-symbol-p (x)
          (and (symbolp x)
               (eq (symbol-package x) *package*))))
    (let ((cells (flatten-to-cells body)))
      (dolist (cons cells)
        (when (local-symbol-p (car cons))
          (setf (car cons)
                (intern (symbol-name (car cons)) package))))
      `(progn ,.body))))

;;;
(defun defpackage-form (package &rest options &key filter)
  "Returns a CL:DEFPACKAGE form for package designated by PACKAGE.

FILTER is a filter function.  Refer to APPLY-FILTER for details.
The filter protocol can be used to influence the symbols which 
end up being listed in the returned DEFPACKAGE form.

Generic function FILTER-EXPORTED-SYMBOLS-ITERATOR is called with
arguments FILTER and PACKAGE to determine which symbols are considered
for exporting."
  (declare (ignore filter))
  (check-type package package-designator "a package designator")
  (setf package (ensure-package package))
  `(cl:defpackage ,(symbol-designator package)
     ,@(spliceable (apply #'package-nicknames-clause package options))
     ,@(spliceable (apply #'package-use-clause package options))
     ,@(spliceable (apply #'package-shadow-clause package options))
     ,@(apply #'package-import-clauses package options)
     ,@(spliceable (apply #'package-export-clause package options))
     ,@(spliceable (package-documentation package))))

;;; Filters 
(defun apply-filter (filter clause-type thing)
  "Applies FILTER to arguments (CLAUSE-TYPE THING); CLAUSE-TYPE is a
keyword symbol denoting the clause of the DEFPACKAGE form where THING
is to appear (:USE, :NICKNAMES, :SHADOW, etc.); THING is, depending on
the clause type, a symbol or a package object.

The FILTER function should return a designator for a list of objects
of the same type as THING.  If the returned value is NIL, THING will
not be included in the clause[**].  If the returned value is a list,
these will be substituted for THING in the DEFPACKAGE clause
CLAUSE-TYPE.  Otherwise the returned value is substituted for THING.
The value NIL designates a pass-through filter, i.e., nothing is
filtered out.

**  The symbol NIL, if it is to be included, should thus be returned as a
list containing the symbol NIL."

  (when (null filter)
    (setf filter #'filter/identity))
  (check-type filter (or symbol function) "a function designator")
  (let ((things (funcall filter clause-type thing)))
    (typecase things
      (null)
      (list
       (mapcar #'symbol-designator things))
      (t
       (list (symbol-designator things))))))

(defun filter/identity (clause-type thing)
  "A filter for APPLY-FILTER which passes through everything."
  (declare (ignore clause-type))
  (or thing '(nil)))

(defgeneric filter-exported-symbols-iterator (filter package)
  (:documentation "Decides which symbols of PACKAGE the FILTER is passed.
  Valid return values are:
    :EXPORTED, designating all exported symbols of a package
    :PACKAGE,  designating all symbols of a package
    :HOME,     designating all symbols whose home package is the package currently worked on.
    :EXTERNAL  a default generic method returns :EXTERNAL.")
  (:method (filter package)
    (declare (ignore package))
   :exported))

(defun package-nicknames-clause (package &key filter)
  "Returns :NICKNAMES clause for PACKAGE."
  (when (package-nicknames package)
    `(:nicknames ,@(loop for name in (package-nicknames package)
                         nconc (apply-filter filter :nicknames name)))))

(defun package-default-use-list ()
  (load-time-value
   (let ((package (make-package (gensym))))
     (prog1 (package-use-list package)
       (delete-package package))) t))

(defun package-use-clause (package &key filter)
  "Returns :USE clause for PACKAGE."
  (let* ((default (package-default-use-list))
         (use-list (package-use-list package))
         (cl-list (load-time-value (list (find-package '#:common-lisp)) t))
         (things (cond ((not (equal default use-list))
                        use-list)
                       ;; heuristics for default use clause
                       ((null default)
                        '())
                       ((equal cl-list default)
                        (list '#:common-lisp)))))
    `(:use ,@(loop for thing in things
                   nconc (apply-filter filter :use thing)))))

(defun package-shadow-clause (package &key filter)
  "Returns :SHADOW clause for PACKAGE."
  (let ((symbols (sort-symbols
                  (loop for symbol in (package-shadowing-symbols package)
                        unless (symbol-importedp symbol package)
                        nconc (apply-filter filter :shadow symbol)))))
    (when symbols
      `(:shadow ,@symbols))))

(defun package-import-clauses (package &key filter)
  "Returns a list of :IMPORT-FROM and :SHADOWING-IMPORT-FROM clauses
for PACKAGE."
  (flet ((clauses (clause-name hashtable)
           (sort-symbols
            (loop for symbols being each hash-value in hashtable
                  using (hash-key package)                    
                  collect
                  (let ((sorted-symbols
                         (sort-symbols
                          (delete-duplicates
                           (loop for s in symbols nconcing
                                 (apply-filter filter clause-name s))
                           :test #'string=))))
                    `(,clause-name ,(symbol-designator package)
                                   ,@sorted-symbols)))
            :key #'second)))
    (let ((package-symbols (make-hash-table :test 'eq))
          (package-shadowing-symbols (make-hash-table :test 'eq)))
      (do-symbols (symbol package)
        (when (symbol-importedp symbol package)
          (push symbol
                (gethash (symbol-package symbol)
                         (if (member symbol (package-shadowing-symbols package))
                             package-shadowing-symbols
                             package-symbols)
                         '()))))
      (append (clauses :import-from package-symbols)
              (clauses :shadowing-import-from package-shadowing-symbols)))))

(defun package-export-clause (package &key filter)
  "Returns :EXPORT clause for PACKAGE."
  (let ((symbols '())
        (iterator (filter-exported-symbols-iterator filter package)))    
    (ecase iterator
      ((:exported)
       (do-external-symbols (symbol package)
         (setf symbols (nconc (apply-filter filter :export symbol)
                              symbols))))
      ((:package :home)
       (do-symbols (symbol package)
         (when (case iterator
                 ((:package) t)
                 ((:home) (eq package (symbol-package symbol))))
           (setf symbols (nconc (apply-filter filter :export symbol)
                                symbols))))))
    (when symbols
      `(:export ,@(sort-symbols symbols)))))

(defun package-documentation (package)
  "Returns the doc-string for package designated by PACKAGE."
  (check-type package package-designator "a package designator")
  (setf package (ensure-package package))
  (let ((docstring (documentation package t)))
    (when docstring
      `(:documentation ,docstring))))

;;;; Utilities
(deftype package-designator ()
  "A package designator"
  `(or package symbol string))

(defun spliceable (thing)
  (if (consp thing)
      (list thing)
      thing))

(defun ensure-package (package-designator)
  "Like FIND-PACKAGE, but signals an error of type PACKAGE-ERROR
if the designated package is not found."
  (check-type package-designator package-designator
              "a package designator")
  (or (find-package package-designator)
      (error 'package-error :package package-designator)))

;;; Symbols
(defgeneric symbol-designator (thing)
  (:documentation "Returns a symbol designator for THING.")
  (:method ((symbol symbol))
    (copy-symbol symbol))
  (:method ((string string))
    (make-symbol string))
  (:method ((package package))
    (make-symbol (package-name package))))

#||
(defmethod symbol-designator :around ((symbol symbol))
  (string symbol))
(defmethod symbol-designator :around ((string string))
  string)
(defmethod symbol-designator :around ((package package))
  (package-name package))
||#

(defun sort-symbols (symbols &rest args &key key &allow-other-keys)
  "Sorts SYMBOLS destructively."
  (declare (ignore key))
  (apply #'sort symbols #'string< args))

(defun symbol-status (symbol &optional package)
  "Returns status of symbol designated by SYMBOL in PACKAGE.
If nil, PACKAGE defaults to *PACKAGE*."
  (setf package (or package *package*))
  (nth-value 1 (find-symbol (symbol-name symbol) package)))

(defun symbol-importedp (symbol &optional package)
  "Returns true if symbol designated by SYMBOL is imported (and not
merely inherited) in PACKAGE.  

If nil, PACKAGE defaults to *PACKAGE*."
  (setf package (ensure-package (or package *package*)))
  (let ((name (symbol-name symbol)))
    (multiple-value-bind (found status)
        (find-symbol name package)
      (and status
           (not (eq (symbol-package found) package))
           (or (eq :internal status)
               (notany (lambda (package)
                         (symbol-exportedp found package))
                       (package-use-list package)))))))

(defun symbol-exportedp (symbol &optional package)
  "Returns true if symbol designated by SYMBOL is exported from PACKAGE.
If nil, PACKAGE defaults to *PACKAGE*."
  (setf package (ensure-package (or package *package*)))
  (multiple-value-bind (symbol status)
      (find-symbol (symbol-name symbol) package)
    (declare (ignore symbol))
    (eq :external status)))

(defun symbol-presentp (symbol &optional package)
  (let ((status (symbol-accessiblep symbol package)))
    (and (not (eq :inherited status))
         status)))

(defun symbol-accessiblep (symbol &optional package)
  (setf package (ensure-package (or package *package*)))
  (multiple-value-bind (symbol status)
      (find-symbol (symbol-name symbol) package)
    (declare (ignore symbol))
    status))

;;;; Pretty Printing
(defun pprint-defpackage (stream defpackage-form)
  (format stream "~:<~W~^ ~3I~:_~W~^~1I~@{~:@_~:<~W~^ ~:I~@_~@{~W~^ ~_~}~:>~}~:>"
          defpackage-form))

(set-pprint-dispatch '(cons (member defpackage)) 'pprint-defpackage)

#||
(set-pprint-dispatch '(cons (member defpackage))
                     'pprint-defpackage)
||#


(defun package-list ()
  (swank:list-all-package-names))

(defun package-count ()
  (length (list-all-packages)))

(defun pprint-package-table ()
  (pprint-tabular t (package-list) t nil 40))

(defun package-export-list (p &optional doc)
  (let (externs)
    (do-external-symbols (x (find-package p))
      (push x externs))
    externs))

(defun package-api-list (p)
    (loop for x in (package-export-list p)
       collect (list x (list :function   (documentation x 'function)
                             :variable   (documentation x 'variable)
                             :type       (documentation x 'type)))))

    
(defun pprint-package-export-list (p)
  (pprint-tabular t (package-export-list p) t nil 32))

(defun package-inquiry-table ()
  (let* ((pkgs (list-all-packages)))
    (loop for p in pkgs
       collect (list (package-name p) (package-export-list p) (package-api-list p)))))

(defun package-symbol-count (&optional (package *package*))
  (iterate:iter 
    (iterate:for      symbol :in-package package)
    (iterate:counting symbol)))

;;
;; (package-symbol-count *package*)
;;

(defun package-symbol-list (&optional (package *package*))
  (iterate:iter 
    (iterate:for      symbol :in-package package)
    (iterate:collecting symbol)))
;;
;; (all-package-symbols)
;;  
(defun package-info (&rest arguments)
  (let ((package nil) (exports (member :exports arguments)))
    (when (car arguments) (setf package (car arguments)))
    (flet
      ((lpac (title plist)
         (when plist
           (setf plist (mapcar
                         (lambda (name) (if (string= "" name) "<empty>" name))
                         (sort (mapcar (lambda (item)
                                         (etypecase item
                                           (string  item)
                                           (symbol  (string item))
                                           (package (package-name item))))
                                 plist)
                           (function string<))))
           (let ((out (format nil "~{~A ~}" plist)))
             (if (< (length out) 60)
               (format t "   ~14A ~A~%" title out)
               (format t "   ~14A~{ ~<~%                  ~1:;~A~>~^~}~%"
                 title plist))))))
      (let* ((packlist
               (if package
                 (list package)
                 (sort (copy-list (list-all-packages))
                   (function string<) :key (function package-name))))
              (name-width
                (loop for p in packlist
                  maximize (length (package-name p))))
              (numb-width
                (loop for p in packlist
                  maximize (truncate
                             (1+ (log (max (length (package-export-list p))
                                        (length (package-symbol-list  p)) 3)
                                   10)))))) 
        (dolist (package packlist)
          (format t "~%~A~%   ~14A ~VD exported, ~VD total.~%"
            (package-name package)
            "Symbols:"
            numb-width (length (package-export-list  package))
            numb-width  (package-symbol-count package)
            (lpac "Nicknames:" (package-nicknames package))
            (lpac "Uses:"      (package-use-list package))
            (lpac "Used by:"   (package-used-by-list package))
            (when exports
              (lpac "Exported:" (package-export-list  package))))
          (values))))))
