;; Opinions differ on how a collection macro should work. There are
;; two major points for discussion: multiple collection variables and
;; implementation method.
;;
;; There are two main ways of implementing collection: sticking
;; successive elements onto the end of the list with tail-collection,
;; and using the PUSH/NREVERSE idiom. Tail-collection is usually
;; faster, except on CLISP, where PUSH/NREVERSE is a little faster.
;;
;; The COLLECTING macro only allows collection into one list, and you
;; can't nest them to get the same effect as multiple collection since
;; it always uses the COLLECT function. If you want to collect into
;; multiple lists, use the WITH-COLLECT macro.


(cl:defpackage :x
  (:use :cl)
  (:export :collect :collecting :with-collectors :set-equal-p))
   
(in-package :x)

;; This should only be called inside of COLLECTING macros, but we
;; define it here to provide an informative error message and to make
;; it easier for SLIME (et al.) to get documentation for the COLLECT
;; function when it's used in the COLLECTING macro.
(def (function e) collect (thing)
  "Collect THING in the context established by the COLLECTING macro"
  (error "Can't collect ~S outside the context of the COLLECTING macro"
	 thing))

(def (macro e) collecting (&body body)
  "Collect things into a list forwards. Within the body of this macro,
the COLLECT function will collect its argument into the list returned
by COLLECTING."
  (with-unique-names (collector tail)
    `(let (,collector ,tail)
      (labels ((collect (thing)
		 (if ,collector
		     (setf (cdr ,tail)
			   (setf ,tail (list thing)))
		     (setf ,collector
			   (setf ,tail (list thing))))))
	,@body)
      ,collector)))

(def (macro e) with-collectors ((&rest collectors) &body body)
  "Collect some things into lists forwards. The names in COLLECTORS
are defined as local functions which each collect into a separate
list.  Returns as many values as there are collectors, in the order
they were given."
  (%with-collectors-check-collectors collectors)
  (let ((gensyms-alist (%with-collectors-gensyms-alist collectors)))
    `(let ,(loop for collector in collectors
		 for tail = (cdr (assoc collector gensyms-alist))
		 nconc (list collector tail))
      (labels ,(loop for collector in collectors
		     for tail = (cdr (assoc collector gensyms-alist))
		     collect `(,collector (thing)
			       (if ,collector
				   (setf (cdr ,tail)
					 (setf ,tail (list thing)))
				   (setf ,collector
					 (setf ,tail (list thing))))))
	,@body)
      (values ,@collectors))))

(defun %with-collectors-check-collectors (collectors)
  "Check that all of the COLLECTORS are symbols. If not, raise an error."
  (let ((bad-collector (find-if-not #'symbolp collectors)))
    (when bad-collector
      (error 'type-error
	     :datum bad-collector
	     :expected-type 'symbol))))

(defun %with-collectors-gensyms-alist (collectors)
  "Return an alist mapping the symbols in COLLECTORS to gensyms"
  (mapcar #'cons collectors
	  (mapcar (compose #'gensym
			   #'(lambda (x)
			       (format nil "~A-TAIL-" x)))
		  collectors)))

;; Some test code which would be too hard to move to the test suite.
#+nil (with-collectors (one-through-nine abc)
	(mapcar #'abc '(a b c))
	(dotimes (x 10)
	  (one-through-nine x)
	  (print one-through-nine))
	(terpri) (terpri))

(def (function e) set-equal-p (list1 list2 &rest args &key test key)
  "Returns t if list1 and list2 are equal (as sets). If list1 and list2 are not
equal returns (as multiple values) nil and two lists. The first list contains the 
elements in list1 and not in list2 and the second list contains elements in list2 
and not in list1."
  (declare (ignore test key))
  (let ((in1-not2 (apply #'set-difference list1 list2 args))
        (in2-not1 (apply #'set-difference list2 list1 args)))
    (if (apply #'union in1-not2 in2-not1 args)
      (values nil in1-not2 in2-not1)
      (values t nil nil))))
