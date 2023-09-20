(in-package #:tz-utilities)

(defsection @alist-utilities
    (:title "Alist utilities")
 "Add-hoc utilities for alist management.

Three operations on alists when they are used as key-value DB need to be defined:
- getting the value for key or nil (assocd),
- updating value for the key, unconditionally (update-alist)
- getting the value and updating it if it does not exist (get-or-update-alist)"
  (assocd function)
  (assocd-for function)
  (clist-to-llist function)
  (get-or-update-alist macro)
  (with-small-cache macro)
  (prune-alist function)
  (update-alist macro))

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

(defun prune-alist (alist test)
  "Remove duplicate entries from an alist."
  (when alist
    (cons (car alist)
	  (prune-alist (remove (caar alist) (cdr alist) :key 'car :test test) test))))


(defmacro with-small-cache ((key &rest pars-test) &body body)
  "Return body, or it cached value. Caching is done on current and
cached KEYs being same; keyword parameters KEY and TEST can modify
what same means.

The cache is implemented as an alist, so should be small to keep efficiency."
  `(let ((cache (load-time-value (cons nil nil))))
     (get-or-update-alist ((cdr cache) ,key ,@pars-test)
       ,@body)))

(declaim (inline assocd assocd-for))

(defun assocd (item alist &rest pars)
  "Convenience shortcut for (cdr (assoc ...)), also known as alist-get in elisp.
FIXME: this must be in some standard library, but I can't find it."
  (cdr (apply #'assoc item alist pars)))

(defun safe-assocd (item alist &rest pars)
  "Convenience shortcut for (cdr (assoc ...)), also known as alist-get in elisp.
FIXME: this must be in some standard library, but I can't find it."
  (apply #'assocd item (remove-if-not 'consp alist) pars))

(defun assocd-for (key &rest pars)
  "Function of one parameter (an alist) that extracts KEY from that alist"
  (lambda (item) (apply #'assocd key item pars)))

(defun clist-to-llist (object)
  "Convert list of conses (e.g., used by hunchentoot) to list of lists
  (e.g., used for display in org mode)."
  (mapcar (lambda (a) (list (car a) (cdr a))) object))
