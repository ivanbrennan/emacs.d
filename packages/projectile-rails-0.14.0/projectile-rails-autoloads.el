;;; projectile-rails-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "projectile-rails" "../../../../../../.emacs.d/packages/projectile-rails-0.14.0/projectile-rails.el"
;;;;;;  "65e98c165f12f3e5e1ca6a5a4428ae22")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/projectile-rails-0.14.0/projectile-rails.el

(autoload 'projectile-rails-mode "projectile-rails" "\
Rails mode based on projectile

\(fn &optional ARG)" t nil)

(autoload 'projectile-rails-on "projectile-rails" "\
Enable `projectile-rails-mode' minor mode if this is a rails project.

\(fn)" nil nil)

(defvar projectile-rails-global-mode nil "\
Non-nil if Projectile-rails-global mode is enabled.
See the `projectile-rails-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `projectile-rails-global-mode'.")

(custom-autoload 'projectile-rails-global-mode "projectile-rails" nil)

(autoload 'projectile-rails-global-mode "projectile-rails" "\
Toggle Projectile-rails mode in all buffers.
With prefix ARG, enable Projectile-rails-global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Projectile-rails mode is enabled in all buffers where
`projectile-rails-on' would do it.
See `projectile-rails-mode' for more information on Projectile-rails mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/projectile-rails-0.14.0/projectile-rails-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/projectile-rails-0.14.0/projectile-rails.el")
;;;;;;  (22770 7749 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; projectile-rails-autoloads.el ends here
