lisplevel7
==========

A library for parsing HL7v2 messages in Common Lisp.

Usage
-----

Given an HL7v2 string like this:

    MSH|^~\&|MERIDIAN|Demo Server|||20100202163120+1100||ORU^R01|XX02021630854-1539|P|2.3.1^AUS&&ISO^AS4700.2&&L|||||AUS
    PID|1||||SMITH^Jessica^^^^^L||19700201|F|||1 Test Street^^WODEN^ACT^2606^AUS^C~2 Test Street^^WODEN^ACT^2606^AUS^C

You get a parsed tree with the HL7Message class:

    (make-instance 'lisplevel7:HL7Message :value text)