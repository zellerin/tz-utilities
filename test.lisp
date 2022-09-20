(fiasco:define-test-package #:tz-utilities-alists
  (:use #:tz-utilities)
  (:import-from #:tz-utilities
                #:*authinfo-file* #:*authinfo-cache*
                #:forget-cached))
(in-package #:tz-utilities-alists)


;;;; Named
(deftest test-named ()
  "Test creation of named package."
  (is (make-instance 'named :name "Holmes"))
  (is (search "Holmes"
              (princ-to-string (make-instance 'named :name "Holmes"))))

  (signals no-name (make-instance 'named)))

;;;; Alist tests
(deftest test-assocd ()
  "Test assocd and assocd-for functions."
  (is (= 1 (assocd :foo '((:foo . 1)))))
  (is (= 1 (funcall (assocd-for :foo) '((:foo . 1))))))

(deftest test-update-alist ()
  "Test scenarios for update-alist macro"
  (let ((alist nil))
    (update-alist alist :foo 1)
    (is (equalp alist `((:foo . 1))))
    (update-alist alist :foo 2)
    (is (equalp alist `((:foo . 2))))
    (update-alist alist :bar 4)
    (is (equalp (assocd :bar alist) 4))
    (is (equalp (length alist) 2))))

(deftest test-clist-llist ()
  (is (equal (clist-to-llist '((1 . 2) (3 . 4)))
             '((1 2) (3 4)))))

;;; Authinfo
(deftest test-get-authinfo ()
  "Several scenarios to look up entry in a mock authinfo file."
  (let ((*authinfo-cache* (make-hash-table
                                         :test (hash-table-test
                                                *authinfo-cache*)))
	(*authinfo-file*
	  (asdf:system-relative-pathname "tz-utilities" "tests/mock-authinfo")))
    (is (equal "test-password" (get-authinfo "dummy" "api")))
    (signals simple-error (get-authinfo "dummy" "noone"))
    (signals simple-error (get-authinfo-both "dummy2"))
    (is (gethash (cons  "dummy" "api") *authinfo-cache*))
    (is (get-authinfo-both "dummy"))
    (setq *authinfo-file* nil) ; test cache - it should not matter that file is nil
    (is (equal "test-password" (get-authinfo "dummy" "api")))
    ;; catch type-error and set password
    (is (equal "added-password"
               (handler-bind
                   ((type-error (lambda (err)
                                  (declare (ignorable err))
                                  (invoke-restart 'use-value
                                                  "added-password"))))
                 (get-authinfo "dummy2" "api"))))
    (is (equal "added-password" (get-authinfo "dummy2" "api")))
    (is (get-authinfo-both "dummy"))))

;;;; Cached vars
(defvar *test-counter*)
(define-cached-var foo (incf *test-counter*))

(deftest cached-vars ()
  (setf *test-counter* 0)
  (forget-cached 'foo)
  (is (equal 1 foo))
  (signals error (forget-cached (gensym)))
  (setf foo 12)
  (is (equal 12 foo)))
