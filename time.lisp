(in-package tz-utilities)

(define-section @time-tools
  "Time related utilities. The baseline and convention is the LOCAL-TIME package."
  (tomorrow)
  (*org-time-format* variable)
  (local-time readtable))

(named-readtables:defreadtable local-time
  "@ reads timestring, #@ universal time."
  (:merge :standard)
  (:macro-char #\@ 'local-time::%read-timestring)
  (:dispatch-macro-char #\# #\@ 'local-time::%read-universal-time))

(defun tomorrow (&optional (n 0) (base (now)))
  "Timestamp of Nth day in future (0 being tomorrow) 00:00."
  (let ((result base))
    (setf (sec-of result) 0)
    (setf (nsec-of result) 0)
    (incf (day-of result) (1+ n))
    result))

(defvar *org-time-format*
  '(:year "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2) " " :short-weekday)
  "Format of timestamp used by Org mode.")
