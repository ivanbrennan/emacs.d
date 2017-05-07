;;; highlight-parentheses-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "highlight-parentheses" "../../../../../../.emacs.d/packages/highlight-parentheses-1.1.0/highlight-parentheses.el"
;;;;;;  "8dad1884e23220c46f144e0c28d21140")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/highlight-parentheses-1.1.0/highlight-parentheses.el

(autoload 'highlight-parentheses-mode "highlight-parentheses" "\
Minor mode to highlight the surrounding parentheses.

\(fn &optional ARG)" t nil)

(defvar global-highlight-parentheses-mode nil "\
Non-nil if Global highlight-parentheses mode is enabled.
See the `global-highlight-parentheses-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-highlight-parentheses-mode'.")

(custom-autoload 'global-highlight-parentheses-mode "highlight-parentheses" nil)

(autoload 'global-highlight-parentheses-mode "highlight-parentheses" "\
Toggle Highlight-parentheses mode in all buffers.
With prefix ARG, enable Global highlight-parentheses mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Highlight-parentheses mode is enabled in all buffers where
`(lambda nil (highlight-parentheses-mode 1))' would do it.
See `highlight-parentheses-mode' for more information on Highlight-parentheses mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/highlight-parentheses-1.1.0/highlight-parentheses-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/highlight-parentheses-1.1.0/highlight-parentheses.el")
;;;;;;  (22770 7674 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; highlight-parentheses-autoloads.el ends here
