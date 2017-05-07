;;; quickrun-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "quickrun" "../../../../../../.emacs.d/packages/quickrun-2.2.8/quickrun.el"
;;;;;;  "11ee2851f2609319811996e6f2031a1e")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/quickrun-2.2.8/quickrun.el

(autoload 'quickrun-set-default "quickrun" "\
Set `key' as default key in programing language `lang'

\(fn LANG KEY)" nil nil)

(autoload 'quickrun-add-command "quickrun" "\


\(fn KEY ALIST &key DEFAULT MODE OVERRIDE)" nil nil)

(autoload 'quickrun "quickrun" "\
Run commands quickly for current buffer
   With universal prefix argument(C-u), select command-key,
   With double prefix argument(C-u C-u), run in compile-only-mode

\(fn &rest PLIST)" t nil)

(autoload 'quickrun-with-arg "quickrun" "\
Run commands quickly for current buffer with arguments

\(fn ARG)" t nil)

(autoload 'quickrun-region "quickrun" "\
Run commands with specified region

\(fn START END)" t nil)

(autoload 'quickrun-replace-region "quickrun" "\
Run commands with specified region and replace

\(fn START END)" t nil)

(autoload 'quickrun-eval-print "quickrun" "\
Run commands with specified region and replace

\(fn START END)" t nil)

(autoload 'quickrun-compile-only "quickrun" "\
Exec only compilation

\(fn)" t nil)

(autoload 'quickrun-shell "quickrun" "\
Run commands in shell for interactive programs

\(fn)" t nil)

(autoload 'quickrun-autorun-mode "quickrun" "\
`quickrun' after saving buffer

\(fn &optional ARG)" t nil)

(autoload 'anything-quickrun "quickrun" "\


\(fn)" t nil)

(autoload 'helm-quickrun "quickrun" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/quickrun-2.2.8/quickrun-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/quickrun-2.2.8/quickrun.el")
;;;;;;  (22770 7718 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; quickrun-autoloads.el ends here
