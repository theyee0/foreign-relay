;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload "cl-charms")
(require 'asdf)
(load "foreign-relay.asd")
(asdf:load-system "foreign-relay")
(setq uiop:*image-entry-point* #'foreign-relay::main)
(uiop:dump-image "foreign-relay.exe" :executable t)
