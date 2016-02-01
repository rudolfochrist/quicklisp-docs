;;;; quicklisp-docs.asd

(asdf:defsystem #:quicklisp-docs
  :description "Generate documentation for quicklisp installed libraries."
  :version "0.1"
  :author "Sebastian Christ <rudolfo.christ@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "quicklisp-docs"))
  :depends-on (#:documentation-template
               #:cl-fad))
