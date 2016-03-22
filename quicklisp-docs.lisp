;;;; quicklisp-docs.lisp

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

(defun make-doc-path (system-name &key (extension "html"))
  (let ((system (asdf:find-system system-name)))
    (merge-pathnames-as-file *ql-docs-home*
                             (pathname-as-file
                              (format nil "~A-~A.~A"
                                      (asdf:component-name system)
                                      (asdf:component-version system)
                                      extension)))))

(defun make-external-symbol-table (package base-path)
  (let ((symbols '())
        (path (namestring base-path)))
    (do-external-symbols (symbol (find-package package) symbols)
      (let ((downcase-symbol (string-downcase (symbol-name symbol))))
        (push (list downcase-symbol
                    (concatenate 'string "file://" path "#" downcase-symbol))
              symbols)))))

(defun write-emacs-file (system symbols)
  (with-open-file (file (make-doc-path system :extension "el")
                        :direction :output
                        :if-exists nil)
    (format file *emacs-lib-template* symbols)))

(defmethod quickload :after (system-specs &key verbose silent prompt explain)
  (declare (ignore verbose silent prompt explain))
  (when (atom system-specs)
    (setf system-specs (list system-specs)))
  (loop for system in system-specs
     unless (member system *excluded-systems*)
     do
       (let ((path (make-doc-path system)))
         (unless (file-exists-p path)
           (when (find-package system)
             (let ((symbols (make-external-symbol-table system path)))
               (unless (null symbols)
                 (create-template system :target path)
                 (write-emacs-file system symbols)
                 (print "Documentation created."))))))))


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
  (pushnew system-designator *excluded-systems*))
