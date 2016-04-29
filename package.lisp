;;;; package.lisp

(defpackage #:quicklisp-docs
  (:nicknames #:ql-docs)
  (:use #:cl :cl-fad)
  (:import-from #:quicklisp
                #:quickload)
  (:export
   #:*ql-docs-home*
   #:*excluded-systems*
   #:*template-path*
   #:remove-outdated-docs
   #:exclude-system))
