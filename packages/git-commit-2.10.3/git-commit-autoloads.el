;;; git-commit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "git-commit" "../../../../../../.emacs.d/packages/git-commit-2.10.3/git-commit.el"
;;;;;;  "3779c81056cd30afabdd66c247f9cc1c")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/git-commit-2.10.3/git-commit.el

(defvar global-git-commit-mode t "\
Non-nil if Global git-commit mode is enabled.
See the `global-git-commit-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-git-commit-mode'.")

(custom-autoload 'global-git-commit-mode "git-commit" nil)

(autoload 'global-git-commit-mode "git-commit" "\
Edit Git commit messages.
This global mode arranges for `git-commit-setup' to be called
when a Git commit message file is opened.  That usually happens
when Git uses the Emacsclient as $GIT_EDITOR to have the user
provide such a commit message.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/git-commit-2.10.3/git-commit-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/git-commit-2.10.3/git-commit.el")
;;;;;;  (22770 7722 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; git-commit-autoloads.el ends here
