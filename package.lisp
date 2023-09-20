;;;; package.lisp

(mgl-pax:define-package #:tz-utilities
  (:use #:cl let-over-lambda local-time cl-store)
  (:import-from #:mgl-pax #:defsection #:section #:macro)
  (:documentation
   "Small general purpose utilities collected from specific projects"))

(in-package #:tz-utilities)

(defsection @overview
    (:title "Overview")
  "This is a random collection of utilities that were not yet extracted to separate
systems.")

(defsection @index
    ()
  (@overview section)
  (@authinfo section)
  (@cached-vars section)
  (@time-tools section)
  (@alist-utilities section)
  (@anaphoric section)
  (@named-objects section)
  (@save-load section)
  (@js-tools section)
  (@debugger-hooks section))
