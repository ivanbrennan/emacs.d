;;; anzu-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "anzu" "../../../../../../.emacs.d/packages/anzu-0.62/anzu.el"
;;;;;;  "9d2ec30b352542bafc0557c974807b82")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/anzu-0.62/anzu.el

(autoload 'anzu-mode "anzu" "\
minor-mode which display search information in mode-line.

\(fn &optional ARG)" t nil)

(defvar global-anzu-mode nil "\
Non-nil if Global anzu mode is enabled.
See the `global-anzu-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-anzu-mode'.")

(custom-autoload 'global-anzu-mode "anzu" nil)

(autoload 'global-anzu-mode "anzu" "\
Toggle Anzu mode in all buffers.
With prefix ARG, enable Global anzu mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Anzu mode is enabled in all buffers where
`anzu--turn-on' would do it.
See `anzu-mode' for more information on Anzu mode.

\(fn &optional ARG)" t nil)

(autoload 'anzu-query-replace-at-cursor "anzu" "\


\(fn)" t nil)

(autoload 'anzu-query-replace-at-cursor-thing "anzu" "\


\(fn)" t nil)

(autoload 'anzu-query-replace "anzu" "\


\(fn ARG)" t nil)

(autoload 'anzu-query-replace-regexp "anzu" "\


\(fn ARG)" t nil)

(autoload 'anzu-replace-at-cursor-thing "anzu" "\


\(fn)" t nil)

(autoload 'anzu-isearch-query-replace "anzu" "\


\(fn ARG)" t nil)

(autoload 'anzu-isearch-query-replace-regexp "anzu" "\


\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/anzu-0.62/anzu-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/anzu-0.62/anzu.el")
;;;;;;  (22774 59847 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; anzu-autoloads.el ends here
