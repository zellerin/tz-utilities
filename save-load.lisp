(in-package #:tz-utilities)

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
  (let ((most-recent-dump (car
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
