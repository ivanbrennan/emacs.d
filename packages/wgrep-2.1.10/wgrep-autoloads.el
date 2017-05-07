;;; wgrep-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "wgrep" "../../../../../../.emacs.d/packages/wgrep-2.1.10/wgrep.el"
;;;;;;  "778b995eab9b5e9fbfb50b60b06fcd01")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/wgrep-2.1.10/wgrep.el

(autoload 'wgrep-setup "wgrep" "\
Setup wgrep preparation.

\(fn)" nil nil)

(add-hook 'grep-setup-hook 'wgrep-setup)

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/wgrep-2.1.10/wgrep-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/wgrep-2.1.10/wgrep.el")
;;;;;;  (22770 7721 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; wgrep-autoloads.el ends here
