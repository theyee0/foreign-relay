;;;; foreign-relay.asd

(asdf:defsystem #:foreign-relay
  :description "A game where you play the role of a outpost intercepting noisy signals and making judgement calls on limited information."
  :author "Jim Chen <theyee0@gmail.com>"
  :license  "GNU GPL v3"
  :version "0.0.1"
  :serial t
  :depends-on ("cl-charms")
  :components ((:file "package")
               (:file "civilization")
               (:file "corruption")
               (:file "event")
               (:file "interface")
               (:file "foreign-relay")))
