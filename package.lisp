;;;; package.lisp

(defpackage #:quicklisp-docs
  (:nicknames #:ql-docs)
  (:use #:cl :cl-fad)
  (:import-from #:quicklisp
                #:quickload)
  (:import-from #:documentation-template
                #:create-template)
  (:export
   #:*ql-docs-home*
   #:*excluded-systems*
   #:remove-outdated-docs
   #:exclude-system))
