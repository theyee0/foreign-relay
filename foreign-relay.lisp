;;;; foreign-relay.lisp

(in-package #:foreign-relay)

(defun print-welcome-message (window)
  (print-words
   "Welcome to Foreign Relay, a game where you play the role of a "
   window))

(defun initialize-civilization ())

(defun main ()
  (charms:with-curses ()
    (when (verify-size)
      (multiple-value-bind (info letters echo entry) (make-interface)
        (charms:get-char entry)))))
