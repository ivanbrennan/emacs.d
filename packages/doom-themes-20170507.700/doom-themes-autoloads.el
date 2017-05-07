;;; doom-themes-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "doom-themes" "../../../../../../.emacs.d/packages/doom-themes-20170507.700/doom-themes.el"
;;;;;;  "af04dc6d7eadeacee0a4f0c58fb11dfc")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/doom-themes-20170507.700/doom-themes.el

(autoload 'doom-color "doom-themes" "\
Retrieve a specific color named NAME (a symbol) from the current DOOM theme.

\(fn NAME)" nil nil)

(autoload 'doom-brighten-minibuffer "doom-themes" "\
Highlight the minibuffer whenever it is in use.

\(fn)" nil nil)

(autoload 'doom-buffer-mode "doom-themes" "\
Brighten source buffers by remapping common faces (like default, hl-line and
linum) to their doom-theme variants.

\(fn &optional ARG)" t nil)

(autoload 'doom-themes-neotree-config "doom-themes" "\
Install DOOM neotree configuration.

\(fn)" nil nil)

(autoload 'doom-themes-nlinum-config "doom-themes" "\
Install DOOM nlinum configuration.

\(fn)" nil nil)

(autoload 'doom-buffer-mode-maybe "doom-themes" "\
Enable `doom-buffer-mode' in the current buffer, if it isn't already and the
buffer represents a real file.

\(fn)" nil nil)

(when (and (boundp 'custom-theme-load-path) load-file-name) (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name))))

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/doom-themes-20170507.700/doom-molokai-theme.el"
;;;;;;  "../../../../../../.emacs.d/packages/doom-themes-20170507.700/doom-neotree.el"
;;;;;;  "../../../../../../.emacs.d/packages/doom-themes-20170507.700/doom-nlinum.el"
;;;;;;  "../../../../../../.emacs.d/packages/doom-themes-20170507.700/doom-one-light-theme.el"
;;;;;;  "../../../../../../.emacs.d/packages/doom-themes-20170507.700/doom-one-theme.el"
;;;;;;  "../../../../../../.emacs.d/packages/doom-themes-20170507.700/doom-themes-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/doom-themes-20170507.700/doom-themes-common.el"
;;;;;;  "../../../../../../.emacs.d/packages/doom-themes-20170507.700/doom-themes-pkg.el"
;;;;;;  "../../../../../../.emacs.d/packages/doom-themes-20170507.700/doom-themes.el")
;;;;;;  (22799 35464 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; doom-themes-autoloads.el ends here
