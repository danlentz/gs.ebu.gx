
(defpackage :x
  (:use :cl)
  (:export
    #:sharp-tilde-reader
    #:flatten
    #:mkstr
    #:symb))

(in-package :x)

(defun segment-reader (stream ch n)
  (if (> n 0)
      (let ((chars))
        (do ((curr (read-char stream)
                   (read-char stream)))
            ((char= ch curr))
          (push curr chars))
        (cons (coerce (nreverse chars) 'string)
              (segment-reader stream ch (- n 1))))))

(defun modifier-reader (stream)
  (let ((char (read-char stream nil)))
    (unread-char char stream)
    (unless (whitespacep char)
      (read-preserving-whitespace stream))))

(defun regex/modifier (regex modifier)
  (format nil "~@[(?~(~a~))~]~a" modifier regex))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))


  (defun group (source n)
    (if (not (listp source)) (error "group: not list"))
    (if (zerop n) (error "zero length"))
    (labels ((rec (source acc)
               (let ((rest (nthcdr n source)))
                 (if (consp rest)
                   (rec rest (cons
                               (subseq source 0 n)
                               acc))
                   (nreverse
                     (cons source acc))))))
      (if source (rec source nil) nil)))

  (defun whitespacep (char)
    (member char '(#\Space #\Tab #\Newline #\Return #\Linefeed) :test #'char=))

  (defun flatten (x)
    (labels ((rec (x acc)
               (cond ((null x) acc)
                 ((atom x) (cons x acc))
                 (t (rec
                      (car x)
                      (rec (cdr x) acc))))))
      (rec x nil)))
  )


(defmacro* match-mode-ppcre-lambda-form (args% modifier)
  ``(lambda (,',str#)
      (cl-ppcre:scan
       ,(regex/modifier (car ,args#) ,modifier)
       ,',str#)))

(defmacro* subst-mode-ppcre-lambda-form (args% modifier%)
  ``(lambda (,',str#)
      (,(if (find #\G (symbol-name ,modifier#))
            'cl-ppcre:regex-replace-all
            'cl-ppcre:regex-replace)
       ,(regex/modifier (car ,args#) (delete #\G (symbol-name ,modifier#)))
       ,',str#
       ,(cadr ,args#))))


(defun sharp-tilde-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((mode-char (read-char stream)))
    (cond
      ((char= mode-char #\m)
       (match-mode-ppcre-lambda-form
        (segment-reader stream
                        (read-char stream)
                        1)
        (modifier-reader stream)))
      ((char= mode-char #\s)
       (subst-mode-ppcre-lambda-form
        (segment-reader stream
                        (read-char stream)
                        2)
        (modifier-reader stream)))
      (t (error "Unknown #~~ mode character")))))
