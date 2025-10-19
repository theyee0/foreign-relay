;;;; package.lisp

(defpackage #:corruption
  (:use #:cl)
  (:export #:corrupt-string #:corrupt-writing))

(defpackage #:civilization
  (:use #:cl)
  (:export #:make-civilization #:civilization-name #:civilization-technology
           #:civilization-relationship #:civilization-strength #:+location-names+
           #:update-relationship #:simulate-war))

(defpackage #:event
  (:use #:cl #:civilization)
  (:export #:generate-salutation #:generate-preamble #:generate-body #:generate-signoff
           #:generate-civilizations))

(defpackage #:interface
  (:use #:cl #:corruption #:event)
  (:export #:verify-size #:print-words #:make-interface #:get-input))

(defpackage #:foreign-relay
  (:use #:cl #:interface #:civilization)
  (:export #:main))
