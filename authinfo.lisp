(in-package #:tz-utilities)

(define-section @authinfo
  "Random convenience utils related to alists, authinfo, and symbols names."
  (get-authinfo) (get-authinfo-both) (*authinfo-file* variable))

;;;; Authinfo
(defvar *authinfo-cache* (make-hash-table :test 'equalp))
(defvar *authinfo-file*  "~/.authinfo")

(defun get-authinfo (machine username)
  "Get secret for machine MACHINE and USERNAME.

Currently, backend is a cache and the .authinfo file(s)."
  (restart-case
   (or (gethash (cons machine username) *authinfo-cache*)
       (let ((from-ss (secret-service:find-the-secret `(("machine" ,machine)
                                                                 ("login" ,username)))))
         (when from-ss
           (setf (gethash (cons machine username) *authinfo-cache*)
                 (secret-service:stringify-secret from-ss))
           (return-from get-authinfo from-ss)))
      (with-open-file (in *authinfo-file*)
	(loop for line = (read-line in nil)
	      while line
	      for system = (nth-value 1 (cl-ppcre:scan-to-strings "machine ([^ 	]*)[ 	].*login ([^ ]*) .*password ([^ ]*)" line))
	      when (and system (equal (aref system 0) machine)
			(equal (aref system 1) username))
		do (return (setf (gethash (cons machine username) *authinfo-cache*)
				 (aref system 2)))))
      (error "Authinfo missing for ~a/~a" machine username))
    (set-password-and-cache (new-pwd)
      :interactive (lambda ()
		     (format *query-io* "Password for ~a on ~a: " username machine)
		     (list (read-line *query-io*)))
      (setf (gethash (cons machine username) *authinfo-cache*)
	    new-pwd))))

(defun get-authinfo-both (to-match)
  "Get user and secret for machine TO-MATCH.

Currently, backend is a cache and the .authinfo file(s)."
  (or (gethash (cons to-match :both) *authinfo-cache*)
      (with-open-file (in *authinfo-file*)
	(loop for line = (read-line in nil)
	      while line
	      for system = (nth-value 1 (cl-ppcre:scan-to-strings "machine ([^ ]*) .*login ([^ ]*) .*password ([^ ]*)" line))
	      when (and system (equal (aref system 0) to-match))
		do (return (setf (gethash (cons to-match :both) *authinfo-cache*)
				 (list (aref system 1) (aref system 2))))))
      (error "Authinfo missing for ~a" to-match)))
