;;;; package.lisp

(defpackage #:foreign-relay
  (:use #:cl))

(defpackage #:corruption
  (:use #:cl)
  (:export #:corrupt-string))
