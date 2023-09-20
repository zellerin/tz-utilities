(in-package #:tz-utilities)

(defsection @authinfo
    (:title "Access to secrets")
  "Utilities to retrieve passwords from safer storage.

The idea is not to have passwords (and possibly related configuration objects -
names, etc) in the code, but elsewhere, and ask for them when needed in a simple way.

Two backends are in place: [netrc/authinfo][ai] files (internally implemented backed, deprecated), and secret service API, using [SECRET-SERVICE][ss] implementation.

In addition this provides caching of the credentials

[ss]: ../secret-service/
[ai]: https://www.gnu.org/software/emacs/manual/html_node/auth/Help-for-users.html"
  "TODO: is caching still needed/useful?"
  "TODO: Packages - link to documentation or source github?"
  "TODO: Emacs now supports pass - should I as well?"
  "TODO: On multiple matching passwords offer restarts for each (unless there is too much)"
  (get-authinfo function)
  (get-authinfo-both function)
  (*authinfo-files* variable)
  (secret-not-found condition))

;;;; Authinfo
(defvar *authinfo-cache* (make-hash-table :test 'equalp))
(defvar *authinfo-files*  '("~/.authinfo")
  "Authinfo files to search secrets in.")

(defun find-by-authinfo (machine username)
  "First password for the machine and username in the authinfo files, or nil.

Special case: if username is NIL, find any record for the machine, and return
list (username password)."
  (dolist (file *authinfo-files*)
    (with-open-file (in file)
      (loop for line = (read-line in nil)
            with res
            while line
            for system = (nth-value 1 (cl-ppcre:scan-to-strings "machine ([^ 	]*)[ 	].*login ([^ ]*) .*password ([^ ]*)" line))
            when (and system (string-equal (aref system 0) machine)
                      (or (null username)
                          (string-equal (aref system 1) username)))
              do
                 (setf res (if username (aref system 2)
                               (list (aref system 1) (aref system 2)))

                       (gethash (cons machine username) *authinfo-cache*) res)
                 (return-from find-by-authinfo res)))))

(defvar *authinfo-hooks*
  (list
   'find-by-authinfo
   (lambda (machine username)
     (let ((from-ss
             (handler-case
                 (secret-service:find-the-secret `(("machine" ,machine)
                                                   ,@ (when username
                                                        `(("login" ,username)))))
               (secret-service:secret-item-search-error ()))))
       (when from-ss
         (secret-service:stringify-secret from-ss)))))
  "List of functions to call to find matching password.")

(define-condition secret-not-found (serious-condition)
  ((login   :accessor get-login   :initarg :login)
   (machine :accessor get-machine :initarg :machine))
  (:report (lambda (object out)
             (with-slots (machine login) object
               (format out "No secret found for ~a~@[/~a~]" machine login))))
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
                   (error 'secret-not-found :machine machine :login username)))
    (use-value (new-pwd)
      :interactive (lambda ()
		     (format *query-io* "Password for ~a on ~a: " username machine)
		     (list (read-line *query-io*)))
      (setf (gethash (cons machine username) *authinfo-cache*) new-pwd))
    ;; TODO: allow store value. Hooks preferred again. SS invocation is
    #+nil (secret-service:create-item "/org/freedesktop/secrets/collection/login" "Test secret"
                   '(("machine" "xxx") ("login" "api")) "xxx")
    ))

(defun get-authinfo-both (machine)
  "Get user and secret for MACHINE.

Store already found secrets in a cache and try to retrieve from cache before
other storage is tried."
  (get-authinfo machine nil))
