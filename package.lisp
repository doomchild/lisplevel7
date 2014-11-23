;;;; package.lisp

(defpackage #:hl7v2
  (:use #:cl)
  (:export :HL7Component)
  (:shadowing-import-from #:split-sequence))

