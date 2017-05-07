;;; gitignore-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "gitignore-mode" "../../../../../../.emacs.d/packages/gitignore-mode-1.2.4/gitignore-mode.el"
;;;;;;  "abdef257ac2a2b5389c22dec85f4f673")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/gitignore-mode-1.2.4/gitignore-mode.el

(autoload 'gitignore-mode "gitignore-mode" "\
A major mode for editing .gitignore files.

\(fn)" t nil)

(dolist (pattern (list "/\\.gitignore\\'" "/info/exclude\\'" "/git/ignore\\'")) (add-to-list 'auto-mode-alist (cons pattern 'gitignore-mode)))

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/gitignore-mode-1.2.4/gitignore-mode-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/gitignore-mode-1.2.4/gitignore-mode.el")
;;;;;;  (22770 7725 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; gitignore-mode-autoloads.el ends here
