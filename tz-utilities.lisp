(in-package #:tz-utilities)

(define-section @alist-utilities
  "Add-hoc utilities for alist management.

Three operations on alists when they are used as key-value DB need to be defined:
- getting the value for key or nil (assocd),
- updating value for the key, unconditionally (update-alist)
- getting the value and updating it if it does not exist (get-or-update-alist)"
  (assocd) (assocd-for)
  (clist-to-llist)
  (get-or-update-alist macro)
  (with-small-cache macro)
  (prune-alist)
  (update-alist modify-macro))

(defun get-or-acons-alist* (alist key new-val-fn assoc-pars)
  "Get value associated with KEY in ALIST, or create record for KEY with value being funcalled NEW-VAL-FN.

Return two values - the new or cached value, and new alist."
  (let ((res (apply #'assocd key alist assoc-pars)))
    (if res
	(values res alist)
	(let ((new-val (funcall new-val-fn)))
	  (values new-val (acons key new-val alist))))))

(defmacro get-or-update-alist ((alist key &rest assoc-pars &key &allow-other-keys) &body new-val-code)
  "Get value associated with KEY in the ALIST; if it is not present,
update ALIST with KEY and evaluate NEW-VAL-CODE as new VALUE."
  `(multiple-value-bind (val new-alist)
       (get-or-acons-alist* ,alist ,key (lambda () ,@new-val-code) ',assoc-pars)
     (setf ,alist new-alist)
     val))

(defun updated-alist (alist key new-value &rest test-pars)
  "Alist with value for KEY "
  (let ((existing (apply #'assoc key alist test-pars)))
    (cond (existing (setf (cdr existing) new-value) alist)
	  (t (acons key new-value alist)))))

(define-modify-macro update-alist (key new-value &rest test-pars) updated-alist
  "Set value for KEY in modified alist to NEW-VALUE")

(defmacro with-small-cache ((key &rest pars-test) &body body)
  "Return body, or it cached value. Caching is done on current and
cached KEYs being same; keyword parameters KEY and TEST can modify
what same means.
o
The cache is implemented as an alist, so should be small to keep efficiency."
  `(let ((cache (load-time-value (cons nil nil))))
     (get-or-update-alist ((cdr cache) ,key ,@pars-test)
       ,@body)))


(defun prune-alist (alist test)
  "Remove duplicate entries from an alist."
  (when alist
    (cons (car alist)
	  (prune-alist (remove (caar alist) (cdr alist) :key 'car :test test) test))))

(declaim (inline assocd assocd-for))

(defun assocd (item alist &rest pars)
  "Convenience shortcut for (cdr (assoc ...)), also known as alist-get in elisp.
FIXME: this must be in some standard library, but I can't find it."
  (cdr (apply #'assoc item alist pars)))

(defun assocd-for (key &rest pars)
  "Function of one parameter (an alist) that extracts KEY from that alist"
  (lambda (item) (apply #'assocd key item pars)))

(defun clist-to-llist (object)
  "Convert list of conses (e.g., used by hunchentoot) to list of lists
  (e.g., used for display in org mode)."
  (mapcar (lambda (a) (list (car a) (cdr a))) object))


(define-section @save-load
  "To preserve relatively stable data across run of the system, the
actual values of the variables can be saved and then loaded on
startup (or anytime else).

The data are saved to a define directory with names created from the
provided string and timestamp; loading is done from the most recent
(based on name) file.

The background mechanism for storing is cl-store.

Typical sequence is

: (defvar *A-VARIABLE* (load-value \"foo\"))
: ...
: ;;; some long calculation to update *A-VARIABLE*
: ...
: (save-value *A-VARIABLE* \"foo\")"
  (save-value)
  (load-value)
  (*default-cache-path* variable))

(defvar *default-cache-path* "~/.cache/lisp/"
  "Default directory to save the variable values.")

(defun save-value (value base-name &key (cache-path *default-cache-path*))
  "Store value to a timestamped storage in cache."
  (cl-store:store value
		  (ensure-directories-exist
		   (local-time:format-timestring nil  (local-time:now)
						 :format `(,cache-path ,base-name
								       "-" ,@local-time:+iso-8601-date-format+ ".store"))))
  (values))

(defun load-value (base-name &key (cache-path *default-cache-path*))
  "Load value keyed by BASE-NAME from the most recent file (by name) in cache."
  (let* ((most-recent-dump (car
			    (sort
			     (directory
			      (format nil "~a~a-????-??-??.store" cache-path base-name))
			     #'string>
			     :key #'pathname-name))))
    (when (null most-recent-dump)
      (warn "No dump file for ~a in ~a" base-name cache-path))
    (and most-recent-dump (cl-store:restore most-recent-dump))))

(defmacro define-loaded-var (var-name (save-identifier &key default)
			     &optional documentation)
  `(progn
     (defvar ,var-name (or (load-value ,save-identifier) ,default) ,documentation)
     (setf (get ',var-name 'save-name) ,save-identifier)))

(define-section @debugger-hooks
  "MAKE-CASCADED-DEBUGGER-HOOK is used to add additional handler cases for current repl.

This is probably not good idea to use and fully experimental."
  (make-cascaded-debugger-hook))

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
