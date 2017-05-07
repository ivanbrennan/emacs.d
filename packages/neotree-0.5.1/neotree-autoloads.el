;;; neotree-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "neotree" "../../../../../../.emacs.d/packages/neotree-0.5.1/neotree.el"
;;;;;;  "4fb47b5a34b94f65f26d9a05ac9b1547")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/neotree-0.5.1/neotree.el

(autoload 'neotree-find "neotree" "\
Quick select node which specified PATH in NeoTree.
If path is nil and no buffer file name, then use DEFAULT-PATH,

\(fn &optional PATH DEFAULT-PATH)" t nil)

(autoload 'neotree-projectile-action "neotree" "\
Integration with `Projectile'.

Usage:
    (setq projectile-switch-project-action 'neotree-projectile-action).

When running `projectile-switch-project' (C-c p p), `neotree' will change root
automatically.

\(fn)" t nil)

(autoload 'neotree-toggle "neotree" "\
Toggle show the NeoTree window.

\(fn)" t nil)

(autoload 'neotree-show "neotree" "\
Show the NeoTree window.

\(fn)" t nil)

(autoload 'neotree-hide "neotree" "\
Close the NeoTree window.

\(fn)" t nil)

(autoload 'neotree-dir "neotree" "\
Show the NeoTree window, and change root to PATH.

\(fn PATH)" t nil)

(defalias 'neotree 'neotree-show "\
Show the NeoTree window.")

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/neotree-0.5.1/neotree-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/neotree-0.5.1/neotree-pkg.el"
;;;;;;  "../../../../../../.emacs.d/packages/neotree-0.5.1/neotree.el")
;;;;;;  (22770 7664 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; neotree-autoloads.el ends here
