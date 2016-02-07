;;;; package.lisp

(defpackage #:quicklisp-docs
  (:use #:cl :cl-fad)
  (:import-from #:quicklisp
                #:quickload)
  (:import-from #:documentation-template
                #:create-template)
  (:export
   #:*ql-docs-home*
   #:remove-outdated-docs))
