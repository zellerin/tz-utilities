(in-package tz-utilities)

(defsection @anaphoric
    (:title "Anaphoric macros")
  "Anaphoric macros not defined in the Let-over-lambda package."
  (awhen macro)
  (it variable))

(defmacro awhen (test-form &body body)
  "Evaluate TEST-FORM, and if true, evaluate BODY with symbol IT bound to its
value."
  `(lol:aif ,test-form
	(progn ,@body)))
