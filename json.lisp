(in-package tz-utilities)

(define-section @js-tools
  "Function on lists that facilitate work with json data after parsing
  them with cl-json. See also LET-ALIST."
  (extract-tags) (extract-tags-from-list)
  (fill-template)
  (fill-template*)
  (json-based-simple class))

(defun extract-tags (alist tag-specs)
  "Extracts values in the ALIST matching keys in TAG-SPECS, optionally applying a
function specified in TAG-SPECS.

TAG-SPECS is a list where each element is either a KEY or list (KEY FN).

Example:
: (extract-tags '((:foo . 1) (:bar 2) (:baz 3)) '(:baz (:foo 1-)))
gives
: ((3) 0)


Maybe LET-ALIST is more natural interface in many cases.

Not optimized for speed:
- iterates tags-specs twice
- conses a lot
- etc."
  (let (tags fns)
    (dolist (tag-spec (reverse tag-specs))
      (cond ((consp tag-spec)
	     (push (car tag-spec) tags)
	     (push (second tag-spec) fns))
	    (t
	     (push tag-spec tags)
	     (push #'identity fns))))
    (labels
	((rec (alist)
	   (cond
	     ((not (consp alist)) nil)
	     ((member (car alist) tags)
	      (list alist))
	     (t (append
		 (rec (car alist))
		 (rec (cdr alist)))))))
      (let ((found (rec alist)))
	(mapcar (lambda (a fn) (funcall fn (assocd a found))) tags fns)))))

(defun extract-tags-from-list (tags data)
  "EXTRACT-TAGS on each item in list DATA."
     (mapcar (alexandria:rcurry #'extract-tags tags)  data))

(defun fill-template (template keys-values)
  "Fill a template suitable for json with values.

Return a copy of template with each cons that starts with a KEY from
KEYS-VALUES plist has its cdr replaced with the appropriate VALUE.

Example:
: (fill-template '(Here is a (:value \"To be filled\"))  ((:value 42)))
gives
: (HERE IS A (:VALUE . 42))"
  (if (consp template)
      (cond ((and (keywordp (car template))
		  (getf keys-values (car template)))
	     (setf (cdr template)
		   (getf keys-values (car template)))
	     template)
	  (t (cons (fill-template (car template) keys-values)
		   (fill-template (cdr template) keys-values))))
    template))

(defun fill-template* (template &rest keys-values)
  "Same as FILL-TEMPLATE, but with KEYS-VALUES as call parameters."
  (fill-template template keys-values))


;;;; Classes to json
(defclass json-based-simple ()
  ()
  (:documentation
   "A class that can be initiated from JSON data. There are two additional initargs,
:slot-map and :json, that maps json path and slot name. Each of keywords in SLOT-MAP is
sought by ALIST in the JSON and if present, both are added to the initargs.

The JSON can be also a slot, of course. And :SLOT-MAP can be in the default-initargs.

Example: define a derived class,
: (defclass json-example (json-based-simple)
:  ((foo :accessor get-foo :initarg :foo)
:   (bar :accessor get-bar :initarg :bar)))

and then use
: (make-instance 'json-example :slot-map
:      '(:foo :bar) :json '(:foo 12))"))

(defmethod shared-initialize :around ((object json-based-simple) slot-names &rest pars
                                      &key json &allow-other-keys)
  (apply #'call-next-method object slot-names
         (append pars
                 (loop for item in json
                       collect (car item)
                       collect (cdr item)))))
