;;; -*- mode: Lisp; common-lisp-style: modern; slime-coding: utf-8-unix -*-
;;;
;;; quicklisp-docs.lisp

(in-package #:quicklisp-docs)

#-quicklisp
(error "This library depends on quicklisp. Please install quikclisp before proceding.")

(defvar *ql-docs-home* (ensure-directories-exist (merge-pathnames #p "docs/"
                                                                  ql:*quicklisp-home*))
  "Directory to install library documentation.")

(defvar *emacs-lib-template*
  "(mapc (lambda (entry)
         (ql-docs-insert (car entry) (cadr entry)))
      '~S)")

(defun replace-slashes (string &optional (char #\-))
  "Replaces every #\/ in STRING with CHAR."
  (ppcre:regex-replace-all "\\/" string (string char)))

(defun make-doc-path (system-name &key (extension "html"))
  (let ((system (asdf:find-system system-name)))
    (merge-pathnames-as-file *ql-docs-home*
                             (pathname-as-file
                              (format nil "~A-~A.~A"
                                      (replace-slashes (asdf:component-name system))
                                      (asdf:component-version system)
                                      extension)))))

(defun make-external-symbol-table (package base-path)
  (let ((symbols '())
        (path (namestring base-path)))
    (do-external-symbols (symbol (find-package package) symbols)
      (push (list (symbol-name symbol)
                  (concatenate 'string
                               "file://" path "#" (symbol-name symbol)))
            symbols))))

(defun write-emacs-file (system symbols)
  (with-open-file (file (make-doc-path system :extension "el")
                        :direction :output
                        :if-exists nil)
    (format file *emacs-lib-template* symbols)))

(defun install-documentation (system)
  "Installs the documentation for SYSTEM."
  (let ((path (make-doc-path system)))
    (when (and (not (file-exists-p path))
               (find-package system))
      (let ((symbols (make-external-symbol-table system path)))
        (unless (null symbols)
          (generate-docs system path)
          (write-emacs-file system symbols)
          (format t "Created documentation for ~A~%" system))))))

(defmethod quickload :after (system-specs &key verbose silent prompt explain)
  (declare (ignore verbose silent prompt explain))
  (when (atom system-specs)
    (setf system-specs (list system-specs)))
  (loop for system in system-specs
        unless (member system *excluded-systems* :test #'string-equal)
          do (progn
               (install-documentation system)
               (loop for dependecy in (asdf:system-depends-on (asdf:find-system system))
                     do (install-documentation (make-keyword (string-upcase dependecy)))))))


(defun remove-outdated-docs ()
  "Removes outdated docs."
  (let ((paths (remove-if (lambda (path)
                            (string= "el" (pathname-type path)))
                          (list-directory *ql-docs-home*))))
    (loop for path in paths
       do (ppcre:do-register-groups (name version)
              (".*/(.*)-(\\d\..*)\.html" (namestring path))
            (let ((system (asdf:find-system name nil)))
              (when (and system
                         (not (string= version
                                       (asdf:component-version system))))
                (delete-file path)
                (delete-file (make-pathname :type "el"
                                            :defaults path))))))))

(defvar *excluded-systems* nil
  "Don't generate documentation for those systems.")

(defun exclude-system (system-designator)
  "Don't create documentation for SYSTEM-DESIGNATOR.
See *EXCLUDED-SYSTEMS* for a list of excluded systems."
  (pushnew (string system-designator) *excluded-systems*))
