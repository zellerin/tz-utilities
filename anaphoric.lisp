(in-package tz-utilities)

(define-section @anaphoric
    "Anaphoric macros not defined in the Let-over-lambda package."
  (awhen)
  (it local-variable))

(defmacro awhen (test-form &body body)
  "Evaluate TEST-FORM, and if true, evaluate BODY with symbol IT bound to its
value."
  `(lol:aif ,test-form
	(progn ,@body)))
