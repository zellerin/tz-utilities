;;;; package.lisp

(mgl-pax:define-package #:tz-utilities
  (:use #:cl let-over-lambda local-time cl-store)
  (:import-from #:mgl-pax #:defsection #:section #:macro)
  (:documentation
   "Small general purpose utilities collected from specific projects"))

(in-package #:tz-utilities)

(defsection @overview
    (:title "Overview")
  "The system contains utilities that I needed in multiple projects that

* I did not find in public repositories and
* are so small that they do not deserve separate package/repository

Not cleaned up and mostly not tested, so probably will not work for you as-is.
")

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
