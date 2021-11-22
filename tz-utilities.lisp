(in-package #:tz-utilities)

(define-section @small-utils
  "Random convenience utils related to alists, authinfo, and symbols names."
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
