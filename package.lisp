;;;; package.lisp

(defpackage #:quicklisp-docs
  (:nicknames #:ql-docs)
  (:use #:cl :cl-fad)
  (:import-from #:quicklisp
                #:quickload)
  (:import-from #:alexandria
                #:make-keyword)
  (:export
   #:*ql-docs-home*
   #:*excluded-systems*
   #:remove-outdated-docs
   #:exclude-system))
