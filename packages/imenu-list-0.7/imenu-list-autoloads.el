;;; imenu-list-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "imenu-list" "../../../../../../.emacs.d/packages/imenu-list-0.7/imenu-list.el"
;;;;;;  "e0d01720e6dac062bae51a179d346b95")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/imenu-list-0.7/imenu-list.el

(autoload 'imenu-list-noselect "imenu-list" "\
Update and show the imenu-list buffer, but don't select it.
If the imenu-list buffer doesn't exist, create it.

\(fn)" t nil)

(autoload 'imenu-list "imenu-list" "\
Update and show the imenu-list buffer.
If the imenu-list buffer doesn't exist, create it.

\(fn)" t nil)

(defvar imenu-list-minor-mode nil "\
Non-nil if Imenu-list minor mode is enabled.
See the `imenu-list-minor-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `imenu-list-minor-mode'.")

(custom-autoload 'imenu-list-minor-mode "imenu-list" nil)

(autoload 'imenu-list-minor-mode "imenu-list" "\
Toggle Imenu-list minor mode on or off.
With a prefix argument ARG, enable Imenu-list minor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{imenu-list-minor-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'imenu-list-smart-toggle "imenu-list" "\
Enable or disable `imenu-list-minor-mode' according to buffer's visibility.
If the imenu-list buffer is displayed in any window, disable
`imenu-list-minor-mode', otherwise enable it.
Note that all the windows in every frame searched, even invisible ones, not
only those in the selected frame.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/imenu-list-0.7/imenu-list-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/imenu-list-0.7/imenu-list.el")
;;;;;;  (22770 7755 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; imenu-list-autoloads.el ends here
