;;; origami-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "origami" "../../../../../../.emacs.d/packages/origami-20170129.805/origami.el"
;;;;;;  "eac452a487f1e5000597bff5e048611b")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/origami-20170129.805/origami.el

(autoload 'origami-mode "origami" "\
Minor mode to selectively hide/show text in the current buffer.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

Lastly, the normal hook `origami-mode-hook' is run using
`run-hooks'.

Key bindings:
\\{origami-mode-map}

\(fn &optional ARG)" t nil)

(defvar global-origami-mode nil "\
Non-nil if Global origami mode is enabled.
See the `global-origami-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-origami-mode'.")

(custom-autoload 'global-origami-mode "origami" nil)

(autoload 'global-origami-mode "origami" "\
Toggle Origami mode in all buffers.
With prefix ARG, enable Global origami mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Origami mode is enabled in all buffers where
`(lambda nil (origami-mode 1))' would do it.
See `origami-mode' for more information on Origami mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/origami-20170129.805/origami-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/origami-20170129.805/origami-parsers.el"
;;;;;;  "../../../../../../.emacs.d/packages/origami-20170129.805/origami-pkg.el"
;;;;;;  "../../../../../../.emacs.d/packages/origami-20170129.805/origami.el")
;;;;;;  (22770 7717 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; origami-autoloads.el ends here
