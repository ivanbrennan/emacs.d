;;; nlinum-relative-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "nlinum-relative" "../../../../../../.emacs.d/packages/nlinum-relative-20160526.8/nlinum-relative.el"
;;;;;;  "dca41de99425eff40a4eb0d3529d7a10")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/nlinum-relative-20160526.8/nlinum-relative.el

(autoload 'nlinum-relative-on "nlinum-relative" "\
Turn ON nlinum-relative.

\(fn)" t nil)

(autoload 'nlinum-relative-off "nlinum-relative" "\
Turn OFF nlinum-relative.

\(fn)" t nil)

(autoload 'nlinum-relative-toggle "nlinum-relative" "\
Toggle between linum-relative and linum.

\(fn)" t nil)

(autoload 'nlinum-relative-mode "nlinum-relative" "\
Display relative line numbers for current buffer.

\(fn &optional ARG)" t nil)

(defvar global-nlinum-relative-mode nil "\
Non-nil if Global nlinum-relative mode is enabled.
See the `global-nlinum-relative-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-nlinum-relative-mode'.")

(custom-autoload 'global-nlinum-relative-mode "nlinum-relative" nil)

(autoload 'global-nlinum-relative-mode "nlinum-relative" "\
Toggle Nlinum-relative mode in all buffers.
With prefix ARG, enable Global nlinum-relative mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Nlinum-relative mode is enabled in all buffers where
`(lambda nil (unless (minibufferp) (nlinum-relative-mode)))' would do it.
See `nlinum-relative-mode' for more information on Nlinum-relative mode.

\(fn &optional ARG)" t nil)

(autoload 'nlinum-relative-setup-evil "nlinum-relative" "\
Setup nlinum-relative-mode for evil

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/nlinum-relative-20160526.8/nlinum-relative-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/nlinum-relative-20160526.8/nlinum-relative.el")
;;;;;;  (22770 7716 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; nlinum-relative-autoloads.el ends here
