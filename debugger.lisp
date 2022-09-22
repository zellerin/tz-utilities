(in-package #:tz-utilities)

(defmacro make-cascaded-debugger-hook (cases)
  "A function suitable for the debugger hook that checks CASES (same format as in
 handler-case) and if none matches, runs whatever was the hook when it was called.

Usage:
: (setq *debugger-hook*
:    (make-cascaded-debugger-hook '((simple-error continue)))))"
  (let* ((err-symbol (gensym "ERROR"))
	 (old-hook-name (gensym "OLD"))
	 (cases-code (mapcar (lambda (case) `(,(car case)
					      ,(if (second case)
						   `(let ((,(car (second case)) ,err-symbol))
						      ,@(cddr case))
						   (cddr case))))
			     cases)))
    `(let ((,old-hook-name ,*debugger-hook*))
       (lambda (,err-symbol old)
	 (declare (ignorable old))
	 (typecase ,err-symbol
	   ((eql :previous) ,old-hook-name)
	   ,@cases-code
	   (t (funcall ,old-hook-name ,err-symbol ,old-hook-name)))))))
