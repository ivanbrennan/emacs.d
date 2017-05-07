;;; git-link-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "git-link" "../../../../../../.emacs.d/packages/git-link-0.4.5/git-link.el"
;;;;;;  "39cb1ef7ce72fa987ea0495a04959847")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/git-link-0.4.5/git-link.el

(autoload 'git-link "git-link" "\
Create a URL representing the current buffer's location in its
GitHub/Bitbucket/GitLab/... repository at the current line number
or active region. The URL will be added to the kill ring. If
`git-link-open-in-browser' is non-`nil' also call `browse-url'.

With a prefix argument prompt for the remote's name.
Defaults to \"origin\".

\(fn REMOTE START END)" t nil)

(autoload 'git-link-commit "git-link" "\
Create a URL representing the commit for the hash under point
in the current buffer's GitHub/Bitbucket/GitLab/...
repository. The URL will be added to the kill ring.

With a prefix argument prompt for the remote's name.
Defaults to \"origin\".

\(fn REMOTE)" t nil)

(autoload 'git-link-homepage "git-link" "\
Create a URL for the current buffer's repository homepage.
The URL will be added to the kill ring. If `git-link-open-in-browser'
is non-`nil' also call `browse-url'.

\(fn REMOTE)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/git-link-0.4.5/git-link-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/git-link-0.4.5/git-link.el")
;;;;;;  (22770 7717 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; git-link-autoloads.el ends here
