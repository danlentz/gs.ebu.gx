;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-
;;;;;
;;;;; gx-user.lisp
;;;;;
;;;;;   initialization of "user" package
;;;;;

(in-package :cl-user)


(defpackage :gx-user
  (:shadowing-import-from :x :collect :collecting :as :a)
  (:use :cl ;:iterate
    :local-time :gx :gx-test :x)
  (:import-from :gx :^^ :list2alt :list2bag :list2seq :write-nt :tunify)
  (:import-from :swank 
    :autodoc :clear-repl-results :connection-info
    :ed-in-emacs :find-definitions-for-emacs :inspect-in-emacs
    :asdf-system-files :list-asdf-systems :list-all-package-names :mop 
    :profile-report :profile-reset :profile-package))



;;;;;
;; Local Variables:
;; indent-tabs: nil
;; outline-regexp: ";;[;]+"
;; End:
;;;;;
