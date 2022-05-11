;;;; tz-utilities.asd
(in-package :asdf-user)

(asdf:defsystem #:tz-utilities
  :description "Describe tz-utilities here"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "Specify license here"
  :version "0.1"
  :serial t
  :depends-on (cz.zellerin.doc let-over-lambda cl-store local-time journal secret-service)
  :components ((:file "package")
               (:file "tz-utilities")
	       (:file "named")
	       (:file "authinfo")
	       (:file "cached-vars")
	       (:file "anaphoric"))
  :in-order-to ((test-op (test-op "tz-utilities/test"))))

(asdf:defsystem #:tz-utilities/test
  :depends-on (tz-utilities lisp-unit)
  :perform (test-op (o s)
                    (uiop:symbol-call :lisp-unit '#:run-tests :all 'tz-utilities))
  :components ((:file "test")))
