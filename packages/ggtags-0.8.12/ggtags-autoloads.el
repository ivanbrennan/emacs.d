;;; ggtags-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "ggtags" "../../../../../../.emacs.d/packages/ggtags-0.8.12/ggtags.el"
;;;;;;  "9a2c81a2c6d78fcee9eec7f4c5965a2f")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/ggtags-0.8.12/ggtags.el

(autoload 'ggtags-find-project "ggtags" "\


\(fn)" nil nil)

(autoload 'ggtags-find-tag-dwim "ggtags" "\
Find NAME by context.
If point is at a definition tag, find references, and vice versa.
If point is at a line that matches `ggtags-include-pattern', find
the include file instead.

When called interactively with a prefix arg, always find
definition tags.

\(fn NAME &optional WHAT)" t nil)

(autoload 'ggtags-mode "ggtags" "\
Toggle Ggtags mode on or off.
With a prefix argument ARG, enable Ggtags mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{ggtags-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'ggtags-build-imenu-index "ggtags" "\
A function suitable for `imenu-create-index-function'.

\(fn)" nil nil)

(autoload 'ggtags-try-complete-tag "ggtags" "\
A function suitable for `hippie-expand-try-functions-list'.

\(fn OLD)" nil nil)

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/ggtags-0.8.12/ggtags-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/ggtags-0.8.12/ggtags.el")
;;;;;;  (22770 7668 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ggtags-autoloads.el ends here
