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
               (:file "alists")
               (:file "debugger")
               (:file "save-load")
               (:file "tz-utilities")
	       (:file "named")
	       (:file "authinfo")
	       (:file "cached-vars")
	       (:file "anaphoric")
               (:file "time"))
  :in-order-to ((test-op (test-op "tz-utilities/test"))))

(asdf:defsystem #:tz-utilities/save-load
  :description "Describe tz-utilities here"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "Specify license here"
  :version "0.1"
  :serial t
  :depends-on (tz-utilities)
  :components ((:file "save-load"))
  :in-order-to ((test-op (test-op "tz-utilities/test/save-load"))))

(asdf:defsystem #:tz-utilities/test
  :depends-on (tz-utilities fiasco)
  :perform (test-op (o s)
                    (uiop:symbol-call :fiasco '#:run-package-tests :package 'tz-utilities-tests))
  :components ((:file "tests/test")))

(asdf:defsystem #:tz-utilities/test/save-load
  :depends-on (tz-utilities fiasco)
  :perform (test-op (o s)
                    (uiop:symbol-call :fiasco '#:run-package-tests :package 'tz-utilities-tests-save-load))
  :components ((:file "tests/save-load")))
