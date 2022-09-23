(in-package tz-utilities)

(define-section @js-tools
  "Function on lists that facilitate work with json data after parsing
  them with cl-json. See also LET-ALIST."
  (extract-tags) (extract-tags-from-list)
  (fill-template)
  (fill-template*)
  (json-based-simple class)
  (json-to-html-dl))

(defun extract-tags (alist tag-specs)
  "Extracts values in the ALIST matching keys in TAG-SPECS, optionally applying a
function specified in TAG-SPECS.

TAG-SPECS is a list where each element is either a KEY or list (KEY FN).

Maybe LET-ALIST is more natural interface in many cases.

Example:
: (extract-tags '((:foo . 1) (:bar 2) (:baz 3)) '(:baz (:foo 1-)))
gives
: ((3) 0)

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
     (mapcar (alexandria:rcurry #'extract-tags tags)  data))

(defun fill-template (template keys-values)
  "Fill a template suitable for json with values.

Return a copy of template with each cons that starts with a KEY from
KEYS-VALUES plist has its cdr replaced with the appropriate VALUE."
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
  (fill-template template keys-values))


;;;; Json to HTML
(defun json-to-html-dl (json out)
  "Format json as a HTML definition list"
  (cl-who:with-html-output (out)
    (:dl
     (dolist (item json)
       (cl-who:htm
	(:dt (cl-who:esc (substitute #\Space #\- (string-capitalize (symbol-name (car item))))))
	(let ((val (format nil "~a" (or (cdr item) "Empty"))))
	  (cl-who:htm
	   (:dd
	    (if (position #\NewLine val)
		(cl-who:htm
		 (:pre
		  (cl-who:esc val)))
		(cl-who:esc val))))))))))




;;;; Classes to json
(defclass json-based-simple ()
  ((slot-map :accessor get-slot-map :initarg :slot-map)
   (json     :accessor get-json     :initarg :json))
  (:documentation
   "A class that can be initiated from JSON data. There is one additional slot, mappings, that maps json path and slot names."))

(defmethod shared-initialize :after ((object json-based-simple) objects &key json &allow-other-keys)
  (when (get-json object)
    (dolist (map (slot-value object 'slot-map))
      (when (assoc (cdr map) json)
	(setf (slot-value object (car map)) (assocd (cdr map) json))))))
