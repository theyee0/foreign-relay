;;;; package.lisp

(defpackage #:corruption
  (:use #:cl)
  (:export #:corrupt-string #:corrupt-writing))

(defpackage #:event
  (:use #:cl)
  (:export #:generate-salutation #:generate-preamble #:generate-body #:generate-signoff
           #:make-entity #:entity-name #:entity-technology #:entity-relationship
           #:entity-strength))

(defpackage #:interface
  (:use #:cl #:corruption #:event)
  (:export #:verify-size #:print-words #:make-interface))

(defpackage #:foreign-relay
  (:use #:cl #:interface)
  (:export #:main))
