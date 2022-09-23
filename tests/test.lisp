(fiasco:define-test-package #:tz-utilities-tests
  (:use #:tz-utilities)
  (:import-from #:tz-utilities
                #:*authinfo-files* #:*authinfo-cache*
                #:forget-cached))
(in-package #:tz-utilities-tests)

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
	(*authinfo-files*
	  (list (asdf:system-relative-pathname "tz-utilities" "tests/mock-authinfo"))))
    (is (equal "test-password" (get-authinfo "dummy" "api")))
    (signals secret-not-found (get-authinfo "dummy" "noone"))
    (signals secret-not-found (get-authinfo-both "dummy2"))
    (is (gethash (cons  "dummy" "api") *authinfo-cache*))
    (is (equalp (get-authinfo-both "dummy")
                '("api" "test-password")))
    (setq *authinfo-files* nil) ; test cache - it should not matter that file is nil
    (is (equal "test-password" (get-authinfo "dummy" "api")))
    ;; catch type-error and set password
    (is (equal "added-password"
               (handler-bind
                   ((secret-not-found (lambda (err)
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


(named-readtables:in-readtable local-time)

(deftest test-tomorrow ()
  (is (local-time:timestamp= @2022-09-21T02:00:00.000000+02:00
                             (tomorrow 0 @2022-09-20T09:47:13.415673+02:00)))
  (is (local-time:timestamp= @2022-09-21T00:00:00.000000Z
                               (tomorrow 0 @2022-09-20T09:47:13.415673Z)))
  (is (local-time:timestamp= @2022-09-24T02:00:00.000000+02:00
                             (tomorrow 3 @2022-09-20T09:47:13.415673+02:00))))

(deftest test-org-time ()
  (is (string-equal (local-time:format-timestring
               nil @2022-09-20T09:47:13.415673Z
               :format tz-utilities:*org-time-format*
               :timezone local-time:+utc-zone+)
              "2022-09-20 09:47 Tue")))

(named-readtables:in-readtable nil)

;;;; awhen
(deftest test-awhen ()
  (is (= 5
         (awhen (+ 1 2)
           (incf it)
           (+ 1 it))))
  (is (null
       (awhen nil 'foo 'bar 'baz))))

;;;; save-load
(deftest test-load-save ()
  (let ((saved-value (random 1000))
        (*default-cache-path* "/tmp/testing/"))
    (flet ((delete-store-files ()
             (mapcar 'delete-file
                     (directory (merge-pathnames "random-test-*.store" *default-cache-path*))))
           (muffled-load ()
             (handler-bind ((warning #'muffle-warning)) (load-value "random-test"))))
      (delete-store-files)
      (is (null (muffled-load)))
      (signals warning (load-value "random-test"))
      (save-value saved-value "random-test")
      (is (= saved-value (load-value "random-test")))
      (delete-store-files)
      (is (null (muffled-load))))))

;;; json

(defclass json-example (json-based-simple)
  ((foo :accessor get-foo :initarg :foo)
   (bar :accessor get-bar :initarg :bar)))

(defclass json-example2 (json-based-simple named-json)
  ((foo :accessor get-foo :initarg :foo)
   (bar :accessor get-bar :initarg :bar)))

(deftest json ()
  (is (equalp (extract-tags '((:foo . 1) (:bar 2) (:baz 3)) '(:baz (:foo 1-)))
              '((3) 0)))
  (is (equalp (fill-template '(Here is a (:value :fill-me))  '(:value 42))
              '(HERE IS A (:VALUE . 42))))
  (let ((json-object
          (make-instance 'json-example
                         :slot-map '(:foo :bar)
                         :json '((:foo . 12)))))
    (is (equal (get-foo json-object) 12))
    (is (null (slot-boundp json-object 'bar)))))
