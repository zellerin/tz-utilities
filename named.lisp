(in-package tz-utilities)

(define-section @named-objects
  "Define a mixin NAMED that streamlines creating \"printable\" named class instances."
  (make-json-object))

(defvar *cntr* 0
  "Counter for naming unnamed instances.")

(export-classes (:slots)
  (defclass named ()
    ((name :accessor get-name :initarg :name
	   :initform nil))
    (:documentation
     "Use as a mixin to ensure that the class has slot NAME that is
     used to print it."))

  (defclass named-json (named)
    ((json :accessor get-json :initarg :json))
    (:documentation "Base class for objects backed up/created from json. Initialize appropriate slots (such as name) in shared-initialize (or initialize-instance).")))

(defmethod print-object ((o named) stream)
  (print-unreadable-object (o stream :type t :identity nil)
    (princ (get-name o) stream)))

(defun make-json-object (json class
			 &key (name-keyword :name)
			   (name-fn (assocd-for name-keyword)))
  (make-instance class :json json :name (funcall name-fn json)))

(defmethod get-name (o)
  "If we have nothing else, let name be object itself."
  o)
