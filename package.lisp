;;;; package.lisp

(cz.zellerin.doc:defpackage #:tz-utilities
  (:use #:cl let-over-lambda cl-store local-time)
  (:sections @small-utils @cached-vars @named)
  (:documentation
   "Small general purpose utilities collected from specific projects"))
