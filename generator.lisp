;;; generator.lisp

(in-package :quicklisp-docs)

(defun accessorp (symbol)
  (fboundp `(setf ,symbol)))

(defun split-documentation (docstring)
  "Splits the DOCSTING on two newlines to preserve paragraphs."
  (flet ((empty-string-p (string)
           (declare (type string string)
                    (optimize speed space))
           (= 0 (length string))))
    (remove-if #'empty-string-p (cl-ppcre:split "\\n{2}" docstring))))

(defun generate-docs (package filespec &key (if-exists :rename-and-delete))
  "Generate HTML documentation for PACKAGE and write it to FILE."
  (setf (who:html-mode) :html5)
  (multiple-value-bind (package-doc symbol-docs)
      (trivial-documentation:package-api package)
    (with-open-file (f filespec :direction :output :if-exists if-exists :if-does-not-exist :create)
      (who:with-html-output (f nil :prologue t :indent t)
        (:html
         (:head
          (:title (who:str (package-name package)))
          (:style (who:str (who:conc
                            "body { margin: 2% 5%; }"
                            "li { margin-top: 5%; list-style: none; }"
                            "p.doc { margin: 2%; }"))))
         (:body
          (:h2 (who:str (package-name package))
               (when package-doc
                 (who:htm (:span (who:fmt "- ~A" package-doc)))))
          (:ul
           (loop for (symb props) on symbol-docs by #'cddr
              do (who:htm
                  (:li
                   (:a :name symb
                       (:p (who:str (who:conc "[" (string (if (accessorp symb)
                                                              :accessor
                                                              (getf (first props) :kind))) "]")))
                       (:p
                        (:b (who:str symb))
                        (:em (who:fmt "~{~A ~}" (getf (first props) :lambda-list)))
                        (when (accessorp symb)
                          (who:htm
                           (:p
                            (who:str "(SETF (")
                            (who:htm (:b (who:str symb)))
                            (who:htm (:em (who:fmt "~{~A~^ ~}"(getf (first props) :lambda-list))))
                            (who:str ") NEW-VALUE)"))))) 
                       (loop for doc in (split-documentation (getf (first props) :documentation))
                          do (who:htm
                              (:p :class "doc"
                                  (who:str doc))))))))))))))
  (values)) 
