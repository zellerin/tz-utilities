(in-package tz-utilities)

(define-section @time-tools
  "Time related utilities. The baseline and convention is the LOCAL-TIME package."
  (tomorrow)
  (*org-time-format* variable))

(defun tomorrow (&optional (n 0))
  "Timestamp of Nth day in future (0 being tomorrow) 00:00."
  (let ((result (now)))
    (setf (sec-of result) (* 3600 24))
    (setf (nsec-of result) 0)
    (incf (day-of result) n)
    result))

(defvar *org-time-format*
  '(:year "-" (:month 2) "-" :day " " (:hour 2) ":" (:min 2) " " :short-weekday)
  "Format of timestamp used by Org mode.")
