;;; quicklisp-docs.el --- Common Look symbol lookup

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Look up documentation either in the Common Lisp HyperSpec or
;; documentation found under ~/quicklisp/docs/.
;; This works with `slime-documentation-lookup' by setting `slime-documentation-lookup-function'.
;; All you have to do is load this file from your Emacs init file and call
;; `ql-docs-reload-docs'.

;;; Code:

(require 'slime)
(require 'hyperspec)
(require 'slime-repl)
(require 'ivy)
(require 'cl-lib)

(defvar ql-docs--symbols (copy-hash-table common-lisp-hyperspec--symbols)
  "Mapping from symbol to URL.  URL can be a relative URL.")

(defvar ql-docs--history nil
  "Quicklisp Docs symbol history.")

(defcustom ql-docs-quicklisp-docs-home "~/quicklisp/docs/"
  "Quicklisp Docs home directory."
  :type '(directory)
  :group 'quicklisp-docs)

(defcustom ql-docs-browser-function browse-url-browser-function
  "Browser function to be used by Quicklisp Docs e.g. `eww-browse-url'."
  :type '(function)
  :group 'quicklisp-docs)

(defun ql-docs-insert (symbol url)
  "Add mapping form SYMBOL to URL."
  (cl-pushnew url
              (gethash symbol ql-docs--symbols)
              :test #'string=))


(defun ql-docs-read-symbol-name (&optional symbol-at-point)
  "Read CL symbol or use SYMBOL-AT-POINT."
  (let* ((symbol-at-point (or symbol-at-point (thing-at-point 'symbol)))
	 (stripped-symbol (and symbol-at-point
			       (common-lisp-hyperspec--strip-cl-package
				(downcase symbol-at-point)))))
    (cond ((and stripped-symbol
		(gethash stripped-symbol ql-docs--symbols))
	   stripped-symbol)
          (t
	   (completing-read "Look up Common Lisp symbol: "
                            ql-docs--symbols nil t
			    stripped-symbol
                            'ql-docs--history)))))
;;;###autoload
(defun ql-docs-lookup-function (symbol-name)
  "Look up SYMBOL-NAME in documentation."
  (interactive (list (ql-docs-read-symbol-name (slime-symbol-at-point))))
  (let ((browse-url-browser-function ql-docs-browser-function))
    (if (common-lisp-hyperspec--find symbol-name)
        (hyperspec-lookup symbol-name)
      (cl-maplist (lambda (entry)
                    (with-temp-buffer
                      (browse-url (car entry))))
                  (gethash symbol-name ql-docs--symbols)))))
;;;###autoload
(setq slime-documentation-lookup-function #'ql-docs-lookup-function)


(defun ql-docs-documentation-files (extension)
  "Returns a list of paths in `ql-docs-quicklisp-docs-home' with EXTENSION."
  (mapcar (lambda (path)
            (concat ql-docs-quicklisp-docs-home path))
          (cl-remove-if-not (lambda (file)
                              (string= extension (file-name-extension file)))
                            (directory-files ql-docs-quicklisp-docs-home))))

;;;###autoload
(defun ql-docs-reload-docs ()
  "Reload documentation for files under `ql-docs-quicklisp-docs-home'."
  (interactive)
  (setq ql-docs--symbols (copy-hash-table common-lisp-hyperspec--symbols))
  (mapc #'load (ql-docs-documentation-files "el")))

;;;###autoload
(defun ql-docs-browse-library-documentation ()
  "Opens the documentation for a selected library."
  (interactive)
  (ivy-read "%d Library: "
            (ql-docs-documentation-files "html")
            :action (lambda (path)
                      (funcall ql-docs-browser-function
                               (concat "file://" (expand-file-name path))))))

(define-key slime-mode-map (kbd "C-c C-d l") #'ql-docs-browse-library-documentation)
(define-key slime-repl-mode-map (kbd "C-c C-d l") #'ql-docs-browse-library-documentation)

(provide 'quicklisp-docs)

;;; quicklisp-docs.el ends here
