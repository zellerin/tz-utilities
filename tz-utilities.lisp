(in-package #:tz-utilities)

(define-section @small-utils
  "Random convenience utils related to alists, and symbols names."
  (assocd) (assocd-for)
  (clist-to-llist))

(declaim (inline assocd assocd-for))

(defun assocd (item alist &rest pars)
  "Convenience shortcut for (cdr (assoc ...)).
FIXME: this must be in some standard library, but I can't find it."
  (cdr (apply #'assoc item alist pars)))

(defun assocd-for (key &rest pars)
  "Return a function of one parameter (an alist) that extracts KEY from that parameter."
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

  (defvar *A-VARIABLE* (load-value \"foo\"))
  ...
  ;;; some long calculation to update *A-VARIABLE*
  ...
  (save-value \"foo\")"
  (save-value)
  (load-value)
  (*default-cache-path* variable))

(defvar *default-cache-path* "~/.cache/lisp/")

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
			      (format nil "~a~a*.store" cache-path base-name))
			     #'string<
			     :key #'pathname-name))))
    (when (null most-recent-dump)
      (warn "No dump file for ~a in ~a" base-name cache-path))
    (and most-recent-dump (cl-store:restore most-recent-dump))))
