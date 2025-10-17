;;;; package.lisp

(defpackage #:foreign-relay
  (:use #:cl))

(defpackage #:corruption
  (:use #:cl)
  (:export #:corrupt-string #:corrupt-writing))

(defpackage #:event
  (:use #:cl #:corruption)
  (:export #:generate-salutation #:generate-preamble #:generate-body #:generate-signoff
           #:make-entity #:entity-name #:entity-technology #:entity-relationship
           #:entity-strength))
