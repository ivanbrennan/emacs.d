;;; coffee-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "coffee-mode" "../../../../../../.emacs.d/packages/coffee-mode-0.6.3/coffee-mode.el"
;;;;;;  "a18a319aa20f0762ea09c939ba113848")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/coffee-mode-0.6.3/coffee-mode.el

(autoload 'coffee-mode "coffee-mode" "\
Major mode for editing CoffeeScript.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))

(add-to-list 'auto-mode-alist '("\\.iced\\'" . coffee-mode))

(add-to-list 'auto-mode-alist '("Cakefile\\'" . coffee-mode))

(add-to-list 'auto-mode-alist '("\\.cson\\'" . coffee-mode))

(add-to-list 'interpreter-mode-alist '("coffee" . coffee-mode))

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/coffee-mode-0.6.3/coffee-mode-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/coffee-mode-0.6.3/coffee-mode.el")
;;;;;;  (22770 7721 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; coffee-mode-autoloads.el ends here
