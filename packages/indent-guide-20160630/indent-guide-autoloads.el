;;; indent-guide-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "indent-guide" "../../../../../../.emacs.d/packages/indent-guide-20160630/indent-guide.el"
;;;;;;  "3b113c265e9e10a8b810b89685e733ab")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/indent-guide-20160630/indent-guide.el

(autoload 'indent-guide-mode "indent-guide" "\
show vertical lines to guide indentation

\(fn &optional ARG)" t nil)

(defvar indent-guide-global-mode nil "\
Non-nil if Indent-guide-global mode is enabled.
See the `indent-guide-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `indent-guide-global-mode'.")

(custom-autoload 'indent-guide-global-mode "indent-guide" nil)

(autoload 'indent-guide-global-mode "indent-guide" "\
Toggle Indent-guide mode in all buffers.
With prefix ARG, enable Indent-guide-global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Indent-guide mode is enabled in all buffers where
`(lambda nil (unless (cl-some (quote derived-mode-p) indent-guide-inhibit-modes) (indent-guide-mode 1)))' would do it.
See `indent-guide-mode' for more information on Indent-guide mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/indent-guide-20160630/indent-guide-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/indent-guide-20160630/indent-guide.el")
;;;;;;  (22770 7716 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; indent-guide-autoloads.el ends here
