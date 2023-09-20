;;;; tz-utilities.asd
(in-package :asdf-user)

(asdf:defsystem #:tz-utilities
  :description "Describe tz-utilities here"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "Specify license here"
  :version "0.1"
  :serial t
  :depends-on (mgl-pax cl-store let-over-lambda local-time secret-service)
  :components ((:file "package")
               (:file "alists")
               (:file "debugger")
	       (:file "named")
	       (:file "authinfo")
	       (:file "cached-vars")
	       (:file "anaphoric")
               (:file "time")
               (:file "json")
               (:file "symbols")
               (:file "save-load"))
  :in-order-to ((test-op (test-op "tz-utilities/test"))))

(asdf:defsystem #:tz-utilities/test
  :depends-on (tz-utilities fiasco)
  :perform (test-op (o s)
                    (uiop:symbol-call :fiasco '#:run-package-tests :package 'tz-utilities-tests))
  :components ((:file "tests/test")))
