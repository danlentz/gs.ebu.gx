;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-
;;;;;
;;;;; seq.lisp
;;;;;
;;;;;   based on pipes.lisp by  Kevin M. Rosenberg
;;;;;   in turn, based on ideas from Peter Norvig's PAIP book
;;;;;
;;;;; Author:      Dan Lentz, Lentz Intergalactic Softworks
;;;;; Maintainer:  <danlentz@gmail.com>
;;;;;

(defpackage :x
  (:use    #:common-lisp)
  (:export #:+empty-seq+ #:make-seq #:seq-tail #:seq-head
           #:do-seq #:map-seq
           #:seq-elt #:seq-enumerate #:seq-values #:seq-force
           #:seq-pipe #:seq-filter #:seq-map #:seq-map-filtering 
           #:seq-mappend #:seq-mappend-filtering))

(in-package :x)

(defconstant +empty-seq+ nil)

(defmacro make-seq (head tail)
  "Create a seq by evaluating head and delaying tail."
  `(cons ,head #'(lambda () ,tail)))

(defun seq-tail (seq)
  "Return tail of seq or list, and destructively update
   the tail if it is a function."
  (if (functionp (rest seq))
    (setf (rest seq) (funcall (rest seq)))
    (rest seq)))

(defun seq-head (seq) (first seq))

(defun seq-elt (seq i)
  "The i-th element of seq, 0-based."
  (if (= i 0)
      (seq-head seq)
    (seq-elt (seq-tail seq) (- i 1))))

(defun seq-enumerate (seq &key count key (result seq))
  "Go through all (or count) elements of seq,
   possibly applying the KEY function. (Try PRINT.)"
  ;; Returns RESULT, which defaults to the seq itself.
  (if (or (eq seq +empty-seq+) (eql count 0))
    result
    (progn
      (unless (null key) (funcall key (seq-head seq)))
      (seq-enumerate (seq-tail seq)
                 :count (if count (1- count))
                 :key key :result result))))

(defun seq-values (seq &optional count)
  "Simple wrapper to return values of a seq"
  (seq-enumerate seq :count count))

(defun seq-force (seq)
  "Force the enumeration of all of the seq. Never returns
if the seq is infinite in length."
  (seq-enumerate seq))

(defun seq-filter (predicate seq)
  "Keep only items in seq satisfying predicate."
  (if (eq seq +empty-seq+)
      +empty-seq+
    (let ((head (seq-head seq))
          (tail (seq-tail seq)))
      (if (funcall predicate head)
          (make-seq head (seq-filter predicate tail))
        (seq-filter predicate tail)))))

(defun seq-map (fn seq)
  "Map fn over seq, delaying all but the first fn call."
  (if (eq seq +empty-seq+)
      +empty-seq+
    (make-seq (funcall fn (seq-head seq))
               (seq-map fn (seq-tail seq)))))

(defun map-seq (fn seq)
  (apply #'seq-map fn seq))

(defmacro do-seq ((elt seq) &body body)
  `(seq-map #'(lambda (,elt) ,body) ,seq))

(defun seq-map-filtering (fn seq &optional filter-pred)
  "Map fn over seq, delaying all but the first fn call,
   while filtering results."
  (if (eq seq +empty-seq+)
      +empty-seq+
    (let* ((head (seq-head seq))
           (tail (seq-tail seq))
           (result (funcall fn head)))
      (if (or (and filter-pred (funcall filter-pred result))
              result)
          (make-seq result (seq-map-filtering fn tail filter-pred))
        (seq-map-filtering fn tail filter-pred)))))

(defun seq-pipe (x y)
  "Return a seq that appends the elements of x and y."
  (if (eq x +empty-seq+)
      y
    (make-seq (seq-head x)
               (seq-pipe (seq-tail x) y))))

(defun seq-mappend (fn seq)
  "Lazily map fn over seq, appending results."
  (if (eq seq +empty-seq+)
      +empty-seq+
    (let ((x (funcall fn (seq-head seq))))
      (make-seq (seq-head x)
                 (seq-pipe (seq-tail x)
                              (seq-mappend fn (seq-tail seq)))))))

(defun seq-mappend-filtering (fn seq &optional filter-pred)
  "Map fn over seq, delaying all but the first fn call,
   appending results while filtering."
  (if (eq seq +empty-seq+)
      +empty-seq+
    (let* ((head (seq-head seq))
           (tail (seq-tail seq))
           (result (funcall fn head)))
      (if (or (and filter-pred (funcall filter-pred result))
              result)
          (make-seq (seq-head result)
                     (seq-pipe (seq-tail result)
                                  (seq-mappend-filtering fn tail filter-pred)))
        (seq-mappend-filtering fn tail filter-pred)))))


;;;
;;;
;;;

(defun integers (&optional (start 0) end)
  (if (or (null end) (<= start end))
      (make-seq start (integers (+ start 1) end))
    nil))

(defun fibgen (a b)
  (make-seq a (fibgen b (+ a b))))

(defun fibs ()
  (fibgen 0 1))

(defun divisible? (x y)
  (zerop (rem x y)))

(defun no-sevens ()
  (seq-filter #'(lambda (x) (not (divisible? x 7))) (integers)))

(defun sieve (stream)
  (make-seq
   (seq-head stream)
   (sieve (seq-filter
           #'(lambda (x)
               (not (divisible? x (seq-head stream))))
           (seq-tail stream)))))

(defun primes ()
  (sieve (integers 2)))

;; Pi

(defun scale-seq (factor seq)
  (seq-map #'(lambda (x) (* x factor)) seq))

(defun sum-seq (sum s)
  (make-seq sum
             (sum-seq (+ sum (seq-head s))
                       (seq-tail s))))

(defun partial-sums (s)
  (make-seq (seq-head s) (sum-seq 0 s)))

(defun pi-summands (n)
  (make-seq (/ 1d0 n)
             (seq-map #'- (pi-summands (+ n 2)))))

(defun pi-stream ()
  (scale-seq 4d0 (partial-sums (pi-summands 1))))

(defun square (x)
  (* x x))

(defun euler-transform (s)
  (let ((s0 (seq-elt s 0))
        (s1 (seq-elt s 1))
        (s2 (seq-elt s 2)))
    (if (and s0 s1 s2)
        (if (eql s1 s2) ;;; series has converged
                +empty-seq+
          (make-seq (- s2 (/ (square (- s2 s1))
                              (+ s0 (* -2 s1) s2)))
                     (euler-transform (seq-tail s))))
          +empty-seq+)))

(defun ln2-summands (n)
  (make-seq (/ 1d0 n)
             (seq-map #'- (ln2-summands (1+ n)))))

(defun ln2-stream ()
  (partial-sums (ln2-summands 1)))

(defun make-tableau (transform s)
  (make-seq s
             (make-tableau transform
                           (funcall transform s))))

(defun accelerated-sequence (transform s)
  (seq-map #'seq-head
            (make-tableau transform s)))

(defun run-seq-test ()
  (let ((*print-length* 20))
    (format t "~&pi-stream:~&  ~S"
            (seq-values (pi-stream) 10))
    (format t "~& pi-stream euler-transform:~&  ~S"
            (seq-values (euler-transform (pi-stream)) 10))
    (format t "~& pi-stream accelerate-sequence:~&  ~S"
            (seq-values
             (accelerated-sequence #'euler-transform (pi-stream)) 10)))
      (format t "~&ln2-stream:~&  ~S"
            (seq-values (ln2-stream) 10))
    (format t "~& ln2-stream euler-transform:~&  ~S"
            (seq-values (euler-transform (ln2-stream)) 10))
    (format t "~& ln2-stream accelerate-sequence:~&  ~S"
            (seq-values
             (accelerated-sequence #'euler-transform (ln2-stream)) 10)))

#||

(run-seq-test)


pi-stream:
  (4.0d0 0.0d0 4.0d0 2.666666666666667d0 3.466666666666667d0
   2.8952380952380956d0 3.3396825396825403d0 2.9760461760461765d0
   3.2837384837384844d0 3.017071817071818d0 3.2523659347188767d0
   . #<Interpreted Closure (:internal x:seq-map) @ #x100c75e052>)
 pi-stream euler-transform:
  (2.0d0 3.0d0 3.166666666666667d0 3.1333333333333337d0
   3.1452380952380956d0 3.13968253968254d0 3.1427128427128435d0
   3.1408813408813416d0 3.142071817071818d0 3.1412548236077655d0
   3.1418396189294033d0
   . #<Interpreted Closure (:internal x::euler-transform) @
       #x100c920712>)
 pi-stream accelerate-sequence:
  (4.0d0 2.0d0 3.2000000000000006d0 3.141944444444445d0
   3.1415958776104405d0 3.1415926884737373d0 3.1415926539063803d0
   3.141592653591136d0 3.141592653588616d0 3.1415926535897953d0 nil
   . #<Interpreted Closure (:internal x:seq-map) @ #x100d0b76f2>)
ln2-stream:
  (1.0d0 0 1.0d0 0.5d0 0.8333333333333333d0 0.5833333333333333d0
   0.7833333333333332d0 0.6166666666666666d0 0.7595238095238095d0
   0.6345238095238095d0 0.7456349206349207d0
   . #<Interpreted Closure (:internal x::sum-seq) @ #x100d189dd2>)
 ln2-stream euler-transform:
  (0.5d0 0.6666666666666666d0 0.7d0 0.6904761904761905d0
   0.6944444444444444d0 0.6924242424242424d0 0.6935897435897436d0
   0.6928571428571428d0 0.6933473389355742d0 0.6930033416875522d0
   0.6932539682539683d0
   . #<Interpreted Closure (:internal x::euler-transform) @
       #x100d321132>)
 ln2-stream accelerate-sequence:
  (1.0d0 0.5d0 0.7083333333333333d0 0.6932487674789901d0
   0.6931482182807979d0 0.6931471911144998d0 0.6931471806449805d0
   0.6931471805603904d0 0.6931471805599309d0 0.6931471805599431d0 nil
. #<Interpreted Closure (:internal x:seq-map) @ #x100da72bc2>)

||#

;;;;;
;; Local Variables:
;; mode: outline-minor
;; indent-tabs: nil
;; outline-regexp: ";;[;]+"
;; End:
