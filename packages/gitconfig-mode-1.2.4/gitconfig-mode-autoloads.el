;;; gitconfig-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "gitconfig-mode" "../../../../../../.emacs.d/packages/gitconfig-mode-1.2.4/gitconfig-mode.el"
;;;;;;  "b5c89b03db7db944cf982336d2b080c0")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/gitconfig-mode-1.2.4/gitconfig-mode.el

(autoload 'gitconfig-mode "gitconfig-mode" "\
A major mode for editing .gitconfig files.

\(fn)" t nil)

(dolist (pattern '("/\\.gitconfig\\'" "/\\.git/config\\'" "/modules/.*/config\\'" "/git/config\\'" "/\\.gitmodules\\'" "/etc/gitconfig\\'")) (add-to-list 'auto-mode-alist (cons pattern 'gitconfig-mode)))

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/gitconfig-mode-1.2.4/gitconfig-mode-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/gitconfig-mode-1.2.4/gitconfig-mode.el")
;;;;;;  (22770 7725 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; gitconfig-mode-autoloads.el ends here
