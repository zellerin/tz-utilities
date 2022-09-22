(in-package tz-utilities)

(define-section @named-objects
  "Class NAMED streamlines creating \"printable\" named class instances.

Class NAMED-JSON allows easier handling and pretty-printing of alist
objects. Word JSON in name is probably a bad one, but this is what I use it for:
alists obtained from cl-json:decode-json calls"
  (named class)
  (get-name generic-fn)
  (no-name condition)
  (named-json class)
  (make-json-object))

(define-condition no-name (serious-condition)
  ()
  (:report "Slot NAME of named object must be defined")
  (:documentation "Condition thrown when an object of class NAMED has no NAME specified at
creation."))

(defgeneric get-name (object)
  (:documentation
   "Name of the object. If it is named object, value of the NAME slot, otherwise
object itself (suitable for strings, numbers, etc.)")
  (:method (object)
    "If we have nothing else, let name be object itself."
    object))

(export-classes (:slots)
  (defclass named ()
    ((name :accessor get-name :initarg :name
	   :initform (restart-case
                         (error 'no-name)
                       (use-value (value)
                         :interactive (lambda () (list (read *query-io*))) value))))
    (:documentation
     "Use as a mixin/base class to ensure that the class has slot NAME. This slot is
used to pretty-print the objects."))

  (defclass named-json (named)
    ((json :accessor get-json :initarg :json))
    (:documentation
     "Objects backed up/created from an alist structure (e.g., decoded json) that
contains, among other, name/display name of the object.")))

(defmethod print-object ((o named) stream)
  (print-unreadable-object (o stream :type t :identity nil)
    (princ (get-name o) stream)))

(defun make-json-object (json class
			 &key (name-keyword :name)
			   (name-fn (assocd-for name-keyword)))
  "Make instance of an object backed by an alist. Typically, sets name based on
the value associated with NAME-KEYWORD, but another function to extract name can
be specified with NAMED-FN."
  (make-instance class :json json :name (funcall name-fn json)))
