;;;; hl7v2.asd

(asdf:defsystem #:hl7v2
  :description "Describe hl7v2 here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:split-sequence)
  :components ((:file "package")
               (:file "hl7v2")))

