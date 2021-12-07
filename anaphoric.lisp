(in-package tz-utilities)

(define-section @anaphoric
    "Anaphoric macros not defined in the lol package"
  (awhen))

(defmacro awhen (test-form &body body)
  `(lol:aif ,test-form
	(progn ,@body)))
