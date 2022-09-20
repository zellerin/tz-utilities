(in-package #:tz-utilities)

(define-section @authinfo
  "Utilities to retrieve passwords from safer storage.

The idea is not to have passwords (and possibly relared configuration objects -
names, etc) in the code, but elsewhere, and ask for them when needed in a simple way.

Two backends are in place: authinfo files, and secret service API."
  (get-authinfo) (get-authinfo-both)
  (*authinfo-file* variable)
  (secret-not-found condition))

;;;; Authinfo
(defvar *authinfo-cache* (make-hash-table :test 'equalp))
(defvar *authinfo-file*  "~/.authinfo"
  "Authinfo file to search secrets in.")

(defun find-by-authinfo (machine username)
  (with-open-file (in *authinfo-file*)
    (loop for line = (read-line in nil)
          while line
          for system = (nth-value 1 (cl-ppcre:scan-to-strings "machine ([^ 	]*)[ 	].*login ([^ ]*) .*password ([^ ]*)" line))
          when (and system (equal (aref system 0) machine)
                    (or (null username) (equal (aref system 1) username)))
            do (return (setf (gethash (cons machine username) *authinfo-cache*)
                             (values (aref system 2)
                                     (aref system 1)))))))

(defvar *authinfo-hooks*
  (list
   #'find-by-authinfo
   (lambda (machine username)
     (let ((from-ss
             (handler-case
                 (secret-service:find-the-secret `(("machine" ,machine)
                                                   ,@ (when username
                                                        `(("login" ,username)))))
               (secret-service:secret-item-search-error ()))))
       (when from-ss
         (secret-service:stringify-secret from-ss))))))

(define-condition secret-not-found (serious-condition)
  ((login   :accessor get-login   :initarg :login)
   (machine :accessor get-machine :initarg :machine))
  (:report (lambda (object out)
             (with-slots (machine login) object
               (format out "No secret found for ~a~[/~a~]" machine login))))
  (:documentation "Condition thrown when no relevant secret is found."))

(defun get-authinfo (machine username)
  "Get secret for machine MACHINE and login USERNAME.

Store already found secrets in a cache and try to retrieve from cache before
other storate is tried.

Raise condition SECRET-NOT-FOUND if relevant secret is not found; it has
established restart USE-VALUE that sets the secret in cache as well."
  (restart-case
   (or (gethash (cons machine username) *authinfo-cache*)
       (loop for auth-fn in *authinfo-hooks*
             for password = (funcall auth-fn machine username)
             when password
               do
                  (setf (gethash (cons machine username) *authinfo-cache*) password)
                  (return password)
             finally
                (error "Authinfo missing for ~a/~a" machine username)))
    (use-value (new-pwd)
      :interactive (lambda ()
		     (format *query-io* "Password for ~a on ~a: " username machine)
		     (list (read-line *query-io*)))
      (setf (gethash (cons machine username) *authinfo-cache*)
	    new-pwd))))

(defun get-authinfo-both (machine)
  "Get user and secret for MACHINE.

Store already found secrets in a cache and try to retrieve from cache before
other storate is tried."
  (get-authinfo machine nil))
