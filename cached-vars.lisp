(in-package #:tz-utilities)

(define-section @cached-vars
  "To prevent long loading times, allow varibles to be initialized
  dynamically - the initialing form is stored at load time, and evaluated when the form is used.

  The \"variable\" symbol is expanded to a form \"(read-cached symbol)\" "
  (define-cached-var macro))

(defstruct (cache
	    (:print-object (lambda (object stream)
			     (when *print-readably*
				 (error 'print-not-readable :object object))
			     (let ((*print-lines* 1)
				   (*print-length* 10))
			       (format stream "<#cached ~a#>"
				       (if (eq object (cache-value object))
					   "Uninitialized"
					   (cache-value object)))))))
  "Information about cached variables."
  initializer load-time value)

(defmethod make-load-form ((s cache) &optional environment)
  (make-load-form-saving-slots s :environment environment
				 :slot-names nil)
#+nil  `(let ((res ,allocate))
     (setf (cache-value res) res
	   (cache-initializer res) ,(cache-initializer s))))

(defvar *cached-vals* nil
  "List of cached values, their  and their initializers. It is used to be able
  to store them eventually.")

(defun find-cached-by-symbol (symbol)
  "Find cached-value structure by variable symbol"
  (getf *cached-vals* symbol))

(defmacro define-cached-var (name value &optional comment)
  "Define NAME as a symbol that is evaluated to VALUE first time it is used."
  (declare (ignore comment))
  (let ((internal-struct
	  (make-cache)))
    `(progn
       (setf (getf *cached-vals* ',name) ,internal-struct
	     (cache-initializer ,internal-struct) (lambda () ,value)
	     (cache-value ,internal-struct) ,internal-struct )
       (eval-when (:load-toplevel :compile-toplevel)
	 (define-symbol-macro ,name
	     (read-cached ,internal-struct))))))

(defun read-cached (struct)
  "If VAL is a delayed value, evaluate it and remember and return result.

Otherwise, return the value directly."
  (let ((val (cache-value struct)))
    (if (cache-p val)
	(let ((time (get-internal-real-time))
	      (res (funcall (cache-initializer val))))
	  (setf (cache-value val) res
		(cache-load-time val)
		(/ (float (- (get-internal-real-time) time))
		   internal-time-units-per-second))
	  res)
	val)))

(defun (setf read-cached) (value object)
  (setf (cache-value object) value))

(defun forget-cached (visible-name)
  (let ((struct (find-cached-by-symbol visible-name)))
    (unless struct
      (error "Cannot forget cached value for ~a - does not exist" visible-name))
    (setf (cache-value struct) struct)
    visible-name))
