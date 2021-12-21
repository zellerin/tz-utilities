(in-package tz-utilities)

(defvar *cntr* 0
  "Counter for naming unnamed instances.")

(export-classes (:slots)
  (defclass named ()
    ((name :accessor get-name :initarg :name
	   :initform (let ((name (format nil "Unnamed instance #~x" (incf *cntr*))))
		       (cerror "Keep generated name for the object"
			       "Should have name: ~a" name)
		       name)))
    (:documentation
     "Use as a mixin to ensure that the class has slot NAME that is
     used to print it."))

  (defmethod print-object ((o named) stream)
    (print-unreadable-object (o stream :type t :identity nil)
      (princ (get-name o) stream))))
