;;; tests.lisp

(in-package #:cl-user)

(defpackage #:quicklisp-docs.test
  (:use :cl :1am :quicklisp-docs :cl-fad)
  (:import-from #:quicklisp-docs
                #:make-doc-path))

(in-package #:quicklisp-docs.test)

(test create-docs
  (let ((*ql-docs-home* #p "/tmp/ql-docs/"))
    (unwind-protect
         (progn
           (ensure-directories-exist *ql-docs-home*)
           (ql:quickload :alexandria)
           (is (probe-file (make-doc-path :alexandria)))
           (is (probe-file (make-doc-path :alexandria :extension "el"))))
      (delete-directory-and-files *ql-docs-home*))))

(defun touch (pathspec)
  (with-open-file (s pathspec :direction :output :if-does-not-exist :create :if-exists :supersede)
    (declare (ignore s))))

(test remove-outdated
  (let ((*ql-docs-home* #p "/tmp/ql-docs/"))
    (unwind-protect
         (progn
           (ensure-directories-exist *ql-docs-home*)
           (ql:quickload :alexandria)
           (touch #P "/tmp/ql-docs/alexandria-1.1.1.1.1.html")
           (touch #P "/tmp/ql-docs/alexandria-1.1.1.1.1.el")
           (is (= 4 (length (list-directory *ql-docs-home*))))
           (remove-outdated-docs)
           (is (= 2 (length (list-directory *ql-docs-home*)))))
      (delete-directory-and-files *ql-docs-home*))))
