(fiasco:define-test-package #:tz-utilities-tests-save-load
  (:use #:tz-utilities))
(in-package #:tz-utilities-tests-save-load)

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
