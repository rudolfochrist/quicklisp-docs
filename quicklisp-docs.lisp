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

(defmethod quickload :after (systems &key verbose silent prompt explain)
  (declare (ignore verbose silent prompt explain))
  (when (atom systems)
    (setf systems (list systems)))
  (loop for system in systems
     do
       (let ((path (make-doc-path system)))
         (unless (file-exists-p path)
           (if (find-package system)
               (let ((symbols (make-external-symbol-table system path)))
                 (unless (null symbols)
                   (create-template system :target path)
                   (write-emacs-file system symbols)
                   (print "Documentation created.")))
               (error "Cannot find package ~A" system))))))


;;; TODO: cleanup docs. This checks for outdated docs in *ql-docs-home* and deletes them
