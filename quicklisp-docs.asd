;;;; quicklisp-docs.asd

(asdf:defsystem #:quicklisp-docs
  :description "Documentation generator for Quicklisp installed libraries."
  :version "0.5.0"
  :author "Sebastian Christ <rudolfo.christ@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/rudolfochrist/quicklisp-docs"
  :source-control (:git "git@github.com:rudolfochrist/quicklisp-docs.git")
  :serial t
  :components ((:file "package")
               (:file "quicklisp-docs")
               (:file "generator"))
  :depends-on (#:trivial-documentation
               #:cl-who
               #:cl-fad
               #:cl-ppcre
               #:alexandria)
  :in-order-to ((test-op (test-op quicklisp-docs.test))))

(asdf:defsystem #:quicklisp-docs.test
  :serial t
  :components ((:file "tests"))
  :depends-on (#:1am
               #:cl-fad
               #:quicklisp-docs)
  :perform (test-op (o c)
                    (uiop:symbol-call :1am :run)))
