;;; nlinum-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "nlinum" "../../../../../../.emacs.d/packages/nlinum-1.6/nlinum.el"
;;;;;;  "b870a559885f0aade602e66cd4bebf21")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/nlinum-1.6/nlinum.el

(autoload 'nlinum-mode "nlinum" "\
Toggle display of line numbers in the left margin (Linum mode).
With a prefix argument ARG, enable Linum mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

Linum mode is a buffer-local minor mode.

\(fn &optional ARG)" t nil)

(defvar global-nlinum-mode nil "\
Non-nil if Global nlinum mode is enabled.
See the `global-nlinum-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-nlinum-mode'.")

(custom-autoload 'global-nlinum-mode "nlinum" nil)

(autoload 'global-nlinum-mode "nlinum" "\
Toggle Nlinum mode in all buffers.
With prefix ARG, enable Global nlinum mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Nlinum mode is enabled in all buffers where
`(lambda nil (unless (minibufferp) (nlinum-mode)))' would do it.
See `nlinum-mode' for more information on Nlinum mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/nlinum-1.6/nlinum-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/nlinum-1.6/nlinum.el")
;;;;;;  (22770 7716 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; nlinum-autoloads.el ends here
