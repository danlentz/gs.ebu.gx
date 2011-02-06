;;; Copyright (c) 2008, Volkan YAZICI <volkan.yazici@gmail.com>
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:

;;; - Redistributions of source code must retain the above copyright notice,
;;;   this list of conditions and the following disclaimer.

;;; - Redistributions in binary form must reproduce the above copyright notice,
;;;   this list of conditions and the following disclaimer in the documentation
;;;   and/or other materials provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.


(cl:defpackage :x
  (:use :cl)
  (:export
   ;; Utilities
   :when-let   :ensure-function   :ensure-list   :with-body-parts
   ;; Definers
   :definer   :available-definer-options   :restricted-definer-options   :initialize-definer
   :expand-definer   :def   :definer-type   :ensure-boolean-option   :ensure-string-option
   :ensure-function-option   :oerror   :validate-definer-options   :combine-option-writers
   :make-option-writer   :has-option-p
   ;; Function Definers
   :declare-optimize   :declare-debug   :initialize-function-like-definer
   :expand-function-like-definer   :function-definer   :macro-definer   :compiler-macro-definer
   :method-definer   :generic-definer   :type-definer   :print-object-definer   :setf-definer
   ;; Variable Definers
   :variable-definer   :constant-definer   :load-time-constant-definer   :special-variable-definer
   :symbol-macro-definer
   ;; Miscelaneous Definers
   :extract-slots   :extract-class-accessors   :extract-struct-accessors   :ensure-slot-spec
   :ensure-slot-spec-initargs   :ensure-slot-spec-accessors   :ensure-slot-spec-readers
   :ensure-slot-spec-writers   :expand-class-like-definer   :class-definer
   :condition-definer :struct-definer))

(cl:in-package :x)

;;; UTILS
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro when-let ((var test) &body body)
    `(let ((,var ,test))
       (when ,var ,@body))))

(defun ensure-function (form)
  (cond ((and (consp form) (member (car form) '(function quote))) (second form))
        ((or (atom form) (and (consp form) (eql (car form) 'lambda))) form)
        (t (error "Invalid ENSURE-FUNCTION input: ~A" form))))

;;; Copied from lists.lisp of alexandria project. (See
;;; http://common-lisp.net/project/alexandria/)
;; (defun ensure-list (list)
;;   "If LIST is a list, it is returned. Otherwise returns the list designated by
;; LIST."
;;   (if (listp list)
;;       list
;;       (list list)))

(defun ensure-list (putative-list)
  (typecase putative-list
    (cl:list putative-list)
    (cl:sequence (coerce putative-list 'cl:list))
    (t (list putative-list))))


;;; Copied from cl-ppcre project of Dr. Edmund Weitz. Reference implementation
;;; posted to comp.lang.lisp as <cy3bshuf30f.fsf@ljosa.com> by Vebjorn Ljosa.
#-:lispworks
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro with-unique-names ((&rest bindings) &body body)
    "Syntax: WITH-UNIQUE-NAMES ( { var | (var x) }* ) declaration* form*

Executes a series of forms with each VAR bound to a fresh,
uninterned symbol. The uninterned symbol is as if returned by a call
to GENSYM with the string denoted by X - or, if X is not supplied, the
string denoted by VAR - as argument.

The variable bindings created are lexical unless special declarations
are specified. The scopes of the name bindings and declarations do not
include the Xs.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
    `(let ,(mapcar #'(lambda (binding)
                       (check-type binding (or cons symbol))
                       (if (consp binding)
                         (destructuring-bind (var x) binding
                           (check-type var symbol)
                           `(,var (gensym ,(etypecase x
                                             (symbol (symbol-name x))
                                             (character (string x))
                                             (string x)))))
                         `(,binding (gensym ,(symbol-name binding)))))
             bindings)
       ,@body)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun parse-body-parts (body)
    "Parses supplied BODY with respect to ([[declaration* | documentation]] form*)
pattern and returns list of DECLARATIONS, DOCUMENTATION and FORM values."
    (loop with declarations
      with documentation
      for forms on body
      for form = (first forms)
      while forms
      do (let ((form (car forms)))
           (cond ((and (listp form)
                    (eql (car form) 'declare))
                   (push form declarations))
             ((and (stringp form)
                (null documentation)
                (cdr forms))
               (setq documentation form))
             (t (loop-finish))))
      finally (return (list (nreverse declarations) documentation forms)))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro with-body-parts ((declarations docstring rest) target-body &body body)
    "Binds passed DECLARATIONS, DOCSTRING and REST to the related parts of the
TARGET-BODY."
    `(destructuring-bind (,declarations ,docstring ,rest)
       (parse-body-parts ,target-body)
       ,@body)))

(defmacro define-print-object ((class &key (identity t) (type t) package)
                               (self &optional stream) &body body)
  "Define a PRINT-OBJECT method using PRINT-UNREADABLE-OBJECT."
  (let ((stream-symbol (or stream (gensym))))
    (with-body-parts (declarations docstring body) body
      `(defmethod print-object ((,self ,class) ,stream-symbol)
         ,@declarations
         ,@(when docstring (list docstring))
         (print-unreadable-object
             (,self ,stream-symbol :type ,type :identity ,identity)
           (let (,@(unless stream `((*standard-output* ,stream-symbol)))
                 ,@(when package `((*package ,(find-package package)))))
             ,@body))))))

;;;


;;; ABSTRACT DEFINER DECLARATIONS

(defclass definer ()
  ((name
    :initarg :name
    :accessor name-of)
   (options
    :initarg :definer-options
    :accessor options-of)
   (forms
    :initarg :forms
    :accessor forms-of))
  (:documentation "Generic class holding definer meta information."))

(define-print-object (definer :identity nil) (self stream)
  (format stream "definer ~s" (name-of self)))


(defun get-definer (name)
    "Returns definer class for the supplied definer of NAME."
  (or (find-class (intern (format nil "~a-definer" name)) :errorp nil)
      (error "Unknown definer: '~a'." name)))

;; (defun get-definer (name)
;;   "Returns definer class for the supplied definer of NAME."
;;   (or (find-class (intern (format nil "~:@(~a-definer~)" name)) :errorp nil)
;;       (error "Unknown definer: `~a'." name)))

(defgeneric available-definer-options (definer)
  (:documentation "List of available options for related definer."))

(defgeneric restricted-definer-options (definer)
  (:documentation
   "List of restricted option combinations for related definer."))

(defgeneric initialize-definer (definer)
  (:documentation "Initializes related definer class slots."))

(defgeneric expand-definer (definer)
  (:documentation "Expands related definer class into its compilation form."))

(defmacro def (name-and-options &rest rest)
  (destructuring-bind (name &rest definer-options) (ensure-list name-and-options)
    (expand-definer
     (initialize-definer (make-instance (get-definer name)
                                        :definer-options definer-options
                                        :forms rest)))))

;;; COMMON ROUTINES

(defun definer-type (definer)
  (class-name (class-of definer)))

(defun ensure-boolean-option (definer keyword value)
  (unless (typep value 'boolean)
    (error "Expecting a boolean instead of ~s for ~s in definer ~s of type ~s."
           value keyword (name-of definer) (definer-type definer)))
  value)

(defun ensure-string-option (definer keyword value)
  (unless (stringp value)
    (error "Expecting a string instead of ~s for ~s in definer ~s of type ~s."
           value keyword (name-of definer) (definer-type definer)))
  value)

(defun ensure-function-option (definer keyword value)
  (declare (ignore definer keyword))
  (if (and (consp value) (member (car value) '(function lambda)))
      value
      `(fdefinition ,value)))

(defun oerror (fmt options definer &rest rest)
  (apply #'error fmt options (name-of definer)
         (definer-type definer) rest))

(defun validate-definer-options (definer &optional extra-options-writer)
  "Validates definer options of function like definers."
  (destructuring-bind (options extra-options)
      ;; Extract definer options.
      (let* ((options (options-of definer))
             (probable-options (coerce (symbol-name (car options)) 'list))
             (available-options (available-definer-options definer)))
        (if (set-difference
             probable-options available-options :test #'char-equal)
            (list nil options)
            (list probable-options (rest options))))
    ;; Check extra options.
    (when (and extra-options (not extra-options-writer))
      (oerror "Invalid definer options ~s in definer ~s of type ~s."
              extra-options definer))
    ;; Check restricted options.
    (dolist (restricted-combination (restricted-definer-options definer))
      (when (every (lambda (option) (member option options :test #'char-equal))
                   restricted-combination)
        (oerror "Ambiguous definer options ~s in definer ~s of type ~s. ~
                 (Cannot use ~s definer options at the same.)"
                options definer restricted-combination)))
    ;; Update validated slot values.
    (setf (options-of definer) options)
    (when extra-options-writer
      (funcall extra-options-writer definer extra-options))))

(defun combine-option-writers (option-writers)
  (lambda (definer options)
    ;; Apply each OPTION-WRITER-FUNCTION, if found appropriate keyword in the
    ;; options of related definer.
    (loop with no-value-p = (gensym)
          for (keyword writer-function) on option-writers by #'cddr
          for option = (getf options keyword no-value-p)
          unless (eql option no-value-p)
          do (progn
               (funcall writer-function definer keyword option)
               (remf options keyword)))
    ;; Check whether we processed all options.
    (unless (null options)
      (oerror "Invalid options ~s for definer ~s of type ~s."
              options definer))))

(defmacro make-option-writer (slot-writer &optional validator)
  (with-unique-names (definer keyword value)
    `(lambda (,definer ,keyword ,value)
       ,@(unless validator
           `((declare (ignore ,keyword))))
       (setf (,slot-writer ,definer)
             ,(if validator
                  `(,validator ,definer ,keyword ,value)
                  value)))))

(defun has-option-p (definer option)
  (member option (options-of definer) :test #'char-equal))

;;;; FUNCTION DEFINER


;;; COMMON VARIABLES

(defclass function-definer (definer)
  ((lambda-list :accessor lambda-list-of)
   (body :accessor body-of)))


;;; COMMON ROUTINES

(defun initialize-function-like-definer (definer &optional extra-options-writer)
  (destructuring-bind (name lambda-list &body body) (forms-of definer)
    (setf (name-of definer) name
          (lambda-list-of definer) lambda-list
          (body-of definer) body))
  (validate-definer-options definer extra-options-writer)
  definer)

(defgeneric declare-optimize (definer)
  (:documentation "Produces optimization declaration for the related definer."))

(defgeneric declare-debug (definer)
  (:documentation "Produces debugging declaration for the related definer."))

(defun expand-function-like-definer (definer function &key method-qualifiers)
  (with-body-parts (declarations docstring body) (body-of definer)
    `(progn
       ,@(when (has-option-p definer #\i)
           `((declaim (inline ,(name-of definer)))))
       (,function
        ,(name-of definer)
        ,@method-qualifiers
        ,(lambda-list-of definer)
         ,@(when docstring `(,docstring))
         ,@(when (has-option-p definer #\o) `(,(declare-optimize definer)))
         ,@(when (has-option-p definer #\d) `(,(declare-debug definer)))
         ,@declarations
         ,@body)
       ,@(when (has-option-p definer #\e) `((export ',(name-of definer)))))))


;;; FUNCTION DEFINER ROUTINES

(defmethod available-definer-options ((definer function-definer))
  (list #\i #\o #\d #\e))

(defmethod restricted-definer-options ((definer function-definer))
  (list (list #\o #\d)))

(defmethod declare-optimize ((definer function-definer))
  `(declare (optimize (speed 3) (debug 0) (safety 2))))

(defmethod declare-debug ((definer function-definer))
  `(declare (optimize (speed 0) (debug 3))))

(defmethod initialize-definer ((definer function-definer))
  (initialize-function-like-definer definer))

(defmethod expand-definer ((definer function-definer))
  (expand-function-like-definer definer 'defun))


;;; MACRO DEFINER ROUTINES

(defclass macro-definer (function-definer) ())

(defmethod available-definer-options ((definer macro-definer))
  (list #\o #\d #\e))

(defmethod expand-definer ((definer macro-definer))
  (expand-function-like-definer definer 'defmacro))


;;; COMPILER-MACRO DEFINER ROUTINES

(defclass compiler-macro-definer (macro-definer) ())

(defmethod expand-definer ((definer compiler-macro-definer))
  (expand-function-like-definer definer 'define-compiler-macro))


;;; METHOD DEFINER ROUTINES

(defclass method-definer (function-definer)
  ((qualifiers :accessor qualifiers-of)))

(defmethod available-definer-options ((definer method-definer))
  (list #\o #\d #\e))

(defmethod initialize-definer ((definer method-definer))
  (initialize-function-like-definer
   definer (lambda (definer extra-options)
             (setf (qualifiers-of definer) extra-options))))

(defmethod expand-definer ((definer method-definer))
  (expand-function-like-definer
   definer 'defmethod :method-qualifiers (qualifiers-of definer)))


;;; GENERIC DEFINER ROUTINES

(defclass generic-definer (function-definer) ())

(defmethod available-definer-options ((definer generic-definer))
  (list #\o #\d #\e))

(defmethod expand-definer ((definer generic-definer))
  (labels ((iter (forms &optional extra-forms declarations)
             (if (endp forms)
                 (mapcar #'reverse (list extra-forms declarations))
                 (let ((form (car forms)))
                   (if (and (consp form) (eql (car form) 'declare))
                       (iter (rest forms)
                             extra-forms
                             (cons form declarations))
                       (iter (rest forms)
                             (cons form extra-forms)
                             declarations))))))
    (destructuring-bind (forms declarations) (iter (body-of definer))
      (let ((name (name-of definer)))
        `(progn
           (defgeneric ,name ,(lambda-list-of definer)
             ,@(when (has-option-p definer #\o) `(,(declare-optimize definer)))
             ,@(when (has-option-p definer #\d) `(,(declare-debug definer)))
             ,@declarations
             ,@forms)
           ,@(when (has-option-p definer #\e) `((export ',name))))))))


;;; TYPE DEFINER ROUTINES

(defclass type-definer (function-definer) ())

(defmethod available-definer-options ((definer type-definer))
  (list #\e))

(defmethod expand-definer ((definer type-definer))
  (expand-function-like-definer definer 'deftype))


;;; PRINT-OBJECT DEFINER ROUTINES

(defclass print-object-definer (function-definer)
  ((print-identity :type boolean :accessor print-identity-of)
   (print-type :type boolean :accessor print-type-of)
   (package :accessor package-of)))

(defmethod available-definer-options ((definer print-object-definer))
  (list #\o #\d))

(defmethod initialize-definer ((definer print-object-definer))
  (prog1 (initialize-function-like-definer
          definer
          (combine-option-writers
           (list
            :print-identity (make-option-writer
                             print-identity-of ensure-boolean-option)
            :print-type (make-option-writer
                         print-type-of ensure-boolean-option)
            :package (make-option-writer package-of))))
    (unless (null (cddr (lambda-list-of definer)))
      (error "Instead of ~s, expecting 2 arguments in the lambda list of ~
              definer ~s of type ~s."
             (lambda-list-of definer) (name-of definer)
             (definer-type definer)))))

(defmethod expand-definer ((definer print-object-definer))
  (with-body-parts (declarations docstring body) (body-of definer)
    `(progn
       (define-print-object
           (,(name-of definer)
            ,@(when (slot-boundp definer 'print-identity)
                `(:identity ,(print-identity-of definer)))
            ,@(when (slot-boundp definer 'print-type)
                `(:type ,(print-type-of definer)))
            ,@(when (slot-boundp definer 'package)
                `(:package ,(package-of definer))))
           ,(lambda-list-of definer)
         ,@(when docstring (list docstring))
         ,@(when (has-option-p definer #\o) `(,(declare-optimize definer)))
         ,@(when (has-option-p definer #\d) `(,(declare-debug definer)))
         ,@declarations
         ,@body))))


;;; SETF DEFINER ROUTINES

(defclass setf-definer (function-definer)
  ((new-value :accessor new-value-of)))

(defmethod available-definer-options ((definer setf-definer))
  (list #\o #\d))

(defmethod initialize-definer ((definer setf-definer))
  (prog1 (initialize-function-like-definer definer)
    (destructuring-bind (new-value-spec &body body) (body-of definer)
      (unless (and (listp new-value-spec)
                   (null (rest new-value-spec))
                   (symbolp (first new-value-spec)))
        (error "Invalid NEW-VALUE symbol in definer ~s of type ~s."
               (name-of definer) (definer-type definer)))
      (setf (new-value-of definer) (first new-value-spec)
            (body-of definer) body))))

(defmethod expand-definer ((definer setf-definer))
  (with-body-parts (declarations documentation body) (body-of definer)
    `(defsetf ,(name-of definer) ,(lambda-list-of definer)
         (,(new-value-of definer))
       ,@(when documentation `(,documentation))
       ,@(when (has-option-p definer #\o) `(,(declare-optimize definer)))
       ,@(when (has-option-p definer #\d) `(,(declare-debug definer)))
       ,@declarations
       ,@body)))


;;; COMMON VARIABLES

(defclass variable-definer (definer)
  ((value :accessor value-of)
   (documentation :accessor documentation-of :initform nil)))

(defmethod available-definer-options ((definer variable-definer))
  (list #\e))

(defmethod restricted-definer-options ((definer variable-definer))
  nil)


;;; COMMON ROUTINES

(defun initialize-variable-like-definer (definer &optional extra-options-writer)
  (destructuring-bind (name &optional (value nil value-p))
      (forms-of definer)
    (setf (name-of definer) name)
    (when value-p (setf (value-of definer) value)))
  (validate-definer-options definer extra-options-writer)
  definer)

(defun validate-constant-initial-value (definer)
  (unless (slot-boundp definer 'value)
    (error "Try to define ~s of type ~s without an initial value."
           (name-of definer) (definer-type definer))))

(defun initialize-and-validate-variable-like-definer
    (definer &optional extra-option-writer-keywords)
  (prog1 (initialize-variable-like-definer
          definer (if extra-option-writer-keywords
                      (combine-option-writers extra-option-writer-keywords)))
    (validate-constant-initial-value definer)))


;;; CONSTANT DEFINER ROUTINES

(defclass constant-definer (variable-definer)
  ((test-function :accessor test-function-of :initform 'eql)))

(defmethod initialize-definer ((definer constant-definer))
  (initialize-and-validate-variable-like-definer
   definer
   (list
    :documentation (make-option-writer documentation-of ensure-string-option)
    :test (make-option-writer test-function-of ensure-function-option))))

(defun reevaluate-constant (definer)
  (let ((name (name-of definer))
        (new-value (value-of definer)))
    `(if (not (boundp ',name))
         ,new-value
         ,(let ((old-value name)
                (test-function (ensure-function (test-function-of definer))))
            `(cond ((not (constantp ',name))
                    (cerror "Try to redefine the variable as a constant."
                            "~@<~s is an already bound non-constant variable ~
                             whose value is ~s.~:@>"
                            ',name ,old-value))
                   ((not (,test-function ,old-value ,new-value))
                    (cerror "Try to redefine the constant."
                            "~@<~s is an already defined constant whose value ~
                             ~s is not equal to the provided initial value ~s ~
                             under ~s.~:@>"
                            ',name ,old-value ',test-function)
                    ,new-value)
                   (t ,old-value))))))

(defmethod expand-definer ((definer constant-definer))
  (let ((name (name-of definer)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defconstant ,name ,(reevaluate-constant definer)
           ,@(when-let (documentation (documentation-of definer))
               `(,documentation))))
       ,@(when (has-option-p definer #\e) `((export ',name))))))


;;; LOAD-TIME-CONSTANT DEFINER ROUTINES

(defclass load-time-constant-definer (variable-definer)
  ((prefix :accessor prefix-of :initform "%")))

(defmethod initialize-definer ((definer load-time-constant-definer))
  (initialize-and-validate-variable-like-definer
   definer
   (list
    :documentation (make-option-writer documentation-of ensure-string-option)
    :prefix (make-option-writer prefix-of ensure-string-option))))

(defmethod expand-definer ((definer load-time-constant-definer))
  (let* ((name (name-of definer))
         (variable-name
          (intern (format nil "~:@(~s~s~)" (prefix-of definer) name))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defvar ,variable-name)
         (setf (documentation ',variable-name 'variable)
               ,(documentation-of definer))
         (unless (boundp ',variable-name)
           (setf ,variable-name ,(value-of definer))))
       (define-symbol-macro ,name (load-time-value ,variable-name))
       ,@(when (has-option-p definer #\e) `((export ',name))))))


;;; SPECIAL-VARIABLE DEFINER ROUTINES

(defclass special-variable-definer (variable-definer) ())

(defmethod initialize-definer ((definer special-variable-definer))
  (initialize-variable-like-definer
   definer
   (combine-option-writers
    (list
     :documentation (make-option-writer documentation-of ensure-string-option)))))

(defmethod expand-definer ((definer special-variable-definer))
  (let ((name (name-of definer))
        (documentation (documentation-of definer)))
    `(progn
       (defvar ,name)
       ,@(when documentation
           `((setf (documentation ',name 'variable) ,documentation)))
       (makunbound ',name)
       ,@(when (slot-boundp definer 'value)
           `((setf ,name ,(value-of definer))))
       ,@(when (has-option-p definer #\e) `((export ',name))))))


;;; SYMBOL-MACRO DEFINER ROUTINES

(defclass symbol-macro-definer (variable-definer) ())

(defmethod initialize-definer ((definer variable-definer))
  (initialize-and-validate-variable-like-definer definer))

(defmethod expand-definer ((definer symbol-macro-definer))
  (let ((name (name-of definer)))
    `(progn
       (define-symbol-macro ,name ,(value-of definer))
       ,@(when (has-option-p definer #\e) `((export ',name))))))

;;; CLASS DEFINER



;;; COMMON ROUTINES

(defun extract-slots (definer)
  (mapcar
   (lambda (spec) (car (ensure-list spec)))
   (funcall (ecase (class-name (class-of definer))
              ((class-definer condition-definer) #'slot-specs-of)
              (struct-definer #'slot-descs-of))
            definer)))

(defun ensure-slot-spec
    (definer test keyword fmt-accessor &optional (package *package*))
  (mapcar
   (lambda (slot-spec)
     (let ((slot-spec (ensure-list slot-spec)))
       (if (funcall test slot-spec)
           slot-spec
           (append
            slot-spec
            (list keyword
                  (intern
                   (format nil (string-upcase (funcall fmt-accessor definer))
                           (first slot-spec))
                   
                   package))))))
   (slot-specs-of definer)))

(defun ensure-slot-spec-initargs (definer)
  (ensure-slot-spec
   definer
   (lambda (slot-spec) (getf (rest slot-spec) :initarg))
   :initarg #'initarg-format-of
   :keyword))

(defun ensure-slot-spec-accessors (definer)
  (ensure-slot-spec
   definer
   (lambda (slot-spec &aux (plist (rest slot-spec)))
     (or (getf plist :accessor)
         (and (getf plist :reader)
              (getf plist :writer))))
   :accessor #'accessor-format-of))

(defun ensure-slot-spec-readers (definer)
  (ensure-slot-spec
   definer
   (lambda (slot-spec) (getf (rest slot-spec) :reader))
   :reader #'reader-format-of))

(defun ensure-slot-spec-writers (definer)
  (ensure-slot-spec
   definer
   (lambda (slot-spec) (getf (rest slot-spec) :writer))
   :writer #'reader-format-of))

(defun expand-class-like-definer (definer function)
  `(progn
     (,function ,(name-of definer) ,(superclasses-of definer)
       ,(slot-specs-of definer)
       ,@(class-options-of definer))
     ,@(when (has-option-p definer #\e) `((export ',(name-of definer))))
     ,@(when (has-option-p definer #\s) `((export ',(extract-slots definer))))
     ,@(when (has-option-p definer #\a)
         `((export ',(extract-class-accessors definer))))
     ,@(when (has-option-p definer #\m)
         (let ((make-sym (intern (format nil "~:@(make-~a~)" (name-of definer)))))
           (with-unique-names (keys)
             `((defun ,make-sym (&rest ,keys &key &allow-other-keys)
                 (apply #'make-instance ',(name-of definer) :allow-other-keys t ,keys))
               ,@(when (has-option-p definer #\e)
                   `((export ',make-sym)))))))))


;;; CLASS DEFINER ROUTINES

(defclass class-definer (definer)
  ((superclasses :accessor superclasses-of)
   (slot-specs :accessor slot-specs-of)
   (class-options :accessor class-options-of)
   (initarg-format
    :accessor initarg-format-of
    :type string
    :initform "~s")
   (accessor-format
    :accessor accessor-format-of
    :type string
    :initform "~s-of")
   (reader-format
    :accessor reader-format-of
    :type string
    :initform "~s-of")
   (writer-format
    :accessor writer-format-of
    :type string
    :initform "~s-of")))

(defmethod available-definer-options ((definer class-definer))
  (list #\e #\a #\s #\n #\c #\r #\w #\m))

(defmethod restricted-definer-options ((definer class-definer))
  nil)

(defmethod initialize-definer ((definer class-definer))
  (destructuring-bind (name superclasses slot-specs &rest class-options)
      (forms-of definer)
    ;; Set (NAME-OF DEFINER) before any OERROR calls.
    (setf (name-of definer) name)
    (unless (listp slot-specs)
      (oerror "Expecting a slot-spec list in options ~s of definer ~
               ~s of type ~s."
              (options-of definer) definer))
    (setf (superclasses-of definer) superclasses
          (slot-specs-of definer) slot-specs
          (class-options-of definer) class-options))
  (validate-definer-options
   definer
   (combine-option-writers
    (list
     :initarg-format (make-option-writer
                      initarg-format-of ensure-string-option)
     :accessor-format (make-option-writer
                       accessor-format-of ensure-string-option)
     :reader-format (make-option-writer
                     reader-format-of ensure-string-option)
     :writer-format (make-option-writer
                     writer-format-of ensure-string-option))))
  (macrolet ((ensure-slot-spec-identifier (option ensure)
               `(when (has-option-p definer ,option)
                  (setf (slot-specs-of definer) (,ensure definer)))))
    (ensure-slot-spec-identifier #\n ensure-slot-spec-initargs)
    (ensure-slot-spec-identifier #\c ensure-slot-spec-accessors)
    (ensure-slot-spec-identifier #\r ensure-slot-spec-readers)
    (ensure-slot-spec-identifier #\w ensure-slot-spec-writers))
  definer)

(defun extract-class-accessors (definer)
  (labels ((get-field (place indicator)
             (when place
               (let ((keyword (first place))
                     (value (second place)))
                 (if (eql keyword indicator)
                     value
                     (get-field (cddr place) indicator))))))
    (remove nil (reduce (lambda (accum slot-spec)
                          (let ((options (rest (ensure-list slot-spec))))
                            (union (list (get-field options :accessor)
                                         (get-field options :writer)
                                         (get-field options :reader))
                                   accum)))
                        (slot-specs-of definer)
                        :initial-value nil))))

(defmethod expand-definer ((definer class-definer))
  (expand-class-like-definer definer 'defclass))


;;; CONDITION DEFINER ROUTINES

(defclass condition-definer (class-definer) ())

(defmethod expand-definer ((definer condition-definer))
  (expand-class-like-definer definer 'define-condition))


;;; STRUCT DEFINER ROUTINES

(defclass struct-definer (definer)
  ((documentation :accessor documentation-of)
   (struct-options :accessor struct-options-of)
   (slot-descs :accessor slot-descs-of)))

(defmethod available-definer-options ((definer struct-definer))
  (list #\e #\a #\s))

(defmethod restricted-definer-options ((definer struct-definer))
  nil)

(defmethod initialize-definer ((definer struct-definer))
  (destructuring-bind (name &rest rest) (forms-of definer)
    (destructuring-bind (documentation &rest slot-descs)
        (if (stringp (car rest)) rest (cons nil rest))
      (setf (name-of definer) (first (ensure-list name))
            (struct-options-of definer) (rest (ensure-list name))
            (documentation-of definer) documentation
            (slot-descs-of definer) slot-descs)))
  (validate-definer-options definer)
  definer)

(defun extract-struct-accessors (definer)
  (mapcar (lambda (slot) (intern (format nil "~:@(~s-~s~)" (name-of definer) slot)))
          (extract-slots definer)))

(defmethod expand-definer ((definer struct-definer))
  `(progn
     (defstruct (,(name-of definer) ,@(struct-options-of definer))
       ,@(when (documentation-of definer) `(,(documentation-of definer)))
       ,@(slot-descs-of definer))
     ,@(when (has-option-p definer #\e) `((export ',(name-of definer))))
     ,@(when (has-option-p definer #\s) `((export ',(extract-slots definer))))
     ,@(when (has-option-p definer #\a)
         `((export ',(extract-struct-accessors definer))))))
