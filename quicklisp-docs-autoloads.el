;;; quicklisp-docs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "quicklisp-docs" "quicklisp-docs.el" (22302
;;;;;;  2746 0 0))
;;; Generated autoloads from quicklisp-docs.el

(autoload 'ql-docs-lookup-function "quicklisp-docs" "\
Look up SYMBOL-NAME in documentation.

\(fn SYMBOL-NAME)" t nil)

(setq slime-documentation-lookup-function #'ql-docs-lookup-function)

(autoload 'ql-docs-reload-docs "quicklisp-docs" "\
Reload documentation for files under `ql-docs-quicklisp-docs-home'.

\(fn)" t nil)

(autoload 'ql-docs-browse-library-documentation "quicklisp-docs" "\
Opens the documentation for a selected library.

\(fn)" t nil)

;;;***

(provide 'quicklisp-docs-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; quicklisp-docs-autoloads.el ends here
