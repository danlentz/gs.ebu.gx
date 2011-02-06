
(in-package :gx-user)

(defpackage #:gx-test
  (:use #:common-lisp)
  (:export #:*epsilon*
    #:*measure*
    #:*significant-figures*
    #:array-error
    #:assert-eq
    #:assert-eql
    #:assert-equal
    #:assert-equality
    #:assert-equalp
    #:assert-error
    #:assert-expands
    #:assert-false
    #:assert-float-equal
    #:assert-norm-equal
    #:assert-number-equal
    #:assert-numerical-equal
    #:assert-prints
    #:assert-rational-equal
    #:assert-sigfig-equal
    #:assert-true
    #:complex-random
    #:default-epsilon
    #:define-test
    #:fail
    #:float-equal
    #:get-test-code
    #:get-tests
    #:logically-equal
    #:make-2d-list
    #:make-random-2d-array
    #:make-random-2d-list
    #:make-random-list
    #:norm
    #:norm-equal
    #:number-equal
    #:numerical-equal
    #:rational-equal
    #:relative-error
    #:relative-error-norm
    #:remove-all-tests
    #:remove-tests
    #:run-all-tests
    #:run-tests
    #:sequence-error
    #:set-equal
    #:sigfig-equal
    #:sump
    #:sumsq
    #:unordered-equal
    #:use-debugger
    #:with-test-listener
    ;; wrap
    
    ;; match

    ;; tables
    ))

(use-package :gx-test :gx-user)
