;;; rbenv-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "rbenv" "../../../../../../.emacs.d/packages/rbenv-0.0.3/rbenv.el"
;;;;;;  "33527d33cd7dce2e1085a3189309327b")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/rbenv-0.0.3/rbenv.el

(autoload 'rbenv-use-global "rbenv" "\
activate rbenv global ruby

\(fn)" t nil)

(autoload 'rbenv-use-corresponding "rbenv" "\
search for .ruby-version and activate the corresponding ruby

\(fn)" t nil)

(autoload 'rbenv-use "rbenv" "\
choose what ruby you want to activate

\(fn RUBY-VERSION)" t nil)

(defvar global-rbenv-mode nil "\
Non-nil if Global rbenv mode is enabled.
See the `global-rbenv-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-rbenv-mode'.")

(custom-autoload 'global-rbenv-mode "rbenv" nil)

(autoload 'global-rbenv-mode "rbenv" "\
use rbenv to configure the ruby version used by your Emacs.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/rbenv-0.0.3/rbenv-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/rbenv-0.0.3/rbenv.el")
;;;;;;  (22770 7748 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; rbenv-autoloads.el ends here
