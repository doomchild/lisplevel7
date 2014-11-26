;;;; hl7v2.asd

(asdf:defsystem #:lisplevel7
  :description "A library for parsing HL7v2 in Common Lisp."
  :author "Lee Crabtree <lee.crabtree@gmail.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:split-sequence
	       #:alexandria
	       #:iterate)
  :components ((:file "package")
               (:file "lisplevel7")))

