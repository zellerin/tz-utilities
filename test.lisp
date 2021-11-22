(in-package tz-utilities)

(eval-when (:compile-toplevel :load-toplevel)
  (use-package 'lisp-unit))

;;;; Named
(define-test named
  (assert-true (make-instance 'named :name "Holmes"))
  (assert-true (search "Holmes"
		       (princ-to-string (make-instance 'named :name "Holmes"))))

  (assert-error 'simple-error
		(make-instance 'named))
  (assert-true
   (search "Unnamed instance"
	   (handler-bind ((simple-error #'continue))
	     (princ-to-string (make-instance 'named))))))

;;;; Small tools
(define-test assocd ()
  (locally
      (declare (notinline assocd assocd-for))
    (assert-equal 1 (assocd :foo '((:foo . 1))))
    (assert-equal 1 (funcall (assocd-for :foo) '((:foo . 1))))))

(define-test clist-llist
  (assert-equal (clist-to-llist '((1 . 2) (3 . 4)))
		'((1 2) (3 4))))

;;; Authinfo
(define-test get-authinfo ()
  (let ((*authinfo-cache* (make-hash-table
			       :test (hash-table-test  *authinfo-cache*)))
	(*authinfo-file*
	  (asdf:system-relative-pathname "tz-utilities" "tests/mock-authinfo") ))
    (assert-equal "test-password" (get-authinfo "dummy" "api"))
    (assert-error 'simple-error (get-authinfo "dummy" "noone"))
    (assert-error 'simple-error (get-authinfo-both "dummy2"))
    (assert-true (gethash (cons  "dummy" "api") *authinfo-cache*))
    (assert-true (get-authinfo-both "dummy"))
    (setq *authinfo-file* nil) ; test cache - it should not matter that file is nil
    (assert-equal "test-password" (get-authinfo "dummy" "api"))
    ;; catch type-error and set password
    (assert-equal "added-password"
		  (handler-bind
		      ((type-error (lambda (err)
				(declare (ignorable err))
				(invoke-restart 'set-password-and-cache
						       "added-password"))))
		    (get-authinfo "dummy2" "api")))
    (assert-equal "added-password" (get-authinfo "dummy2" "api"))
    (assert-true (get-authinfo-both "dummy"))))

;;;; Cached vars
(defvar *test-counter*)
(define-cached-var foo (incf *test-counter*))

(define-test cached-vars ()
  (setf *test-counter* 0)
  (forget-cached 'foo)
  (assert-equal 1 foo)
  (assert-equal 1 foo)
  (assert-error 'error (forget-cached (gensym)))
  (setf foo 12)
  (assert-equal 12 foo))
