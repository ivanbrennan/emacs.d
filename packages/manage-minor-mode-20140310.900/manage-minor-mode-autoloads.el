;;; manage-minor-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "manage-minor-mode" "../../../../../../.emacs.d/packages/manage-minor-mode-20140310.900/manage-minor-mode.el"
;;;;;;  "8da4921b51b33be90054d9cc409823bc")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/manage-minor-mode-20140310.900/manage-minor-mode.el

(autoload 'manage-minor-mode-set "manage-minor-mode" "\


\(fn)" nil nil)

(autoload 'manage-minor-mode "manage-minor-mode" "\


\(fn &optional $LAST-TOGGLED-ITEM)" t nil)

(autoload 'manage-minor-mode-bals "manage-minor-mode" "\

Eradicate all minor-modes in the current buffer.
This command may cause unexpected effect even to other buffers.
However, don't worry, restore command exists:
 `manage-minor-mode-restore-from-bals'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/manage-minor-mode-20140310.900/manage-minor-mode-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/manage-minor-mode-20140310.900/manage-minor-mode.el")
;;;;;;  (22770 7717 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; manage-minor-mode-autoloads.el ends here
