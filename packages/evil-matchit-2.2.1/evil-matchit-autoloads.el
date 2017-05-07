;;; evil-matchit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "evil-matchit" "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit.el"
;;;;;;  "2e4161d359a814071b1dee790df93712")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit.el

(autoload 'evilmi-select-items "evil-matchit" "\
Select items/tags and the region between them.

\(fn &optional NUM)" t nil)

(autoload 'evilmi-delete-items "evil-matchit" "\
Delete items/tags and the region between them.

\(fn &optional NUM)" t nil)

(autoload 'evilmi-jump-to-percentage "evil-matchit" "\
Like Vim %.

\(fn NUM)" t nil)

(autoload 'evilmi-jump-items "evil-matchit" "\
Jump between items.

\(fn &optional NUM)" t nil)

(autoload 'evilmi-version "evil-matchit" "\


\(fn)" t nil)

(autoload 'evil-matchit-mode "evil-matchit" "\
Buffer-local minor mode to emulate matchit.vim.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-evil-matchit-mode "evil-matchit" "\
Enable evil-matchit-mode in the current buffer.

\(fn)" nil nil)

(autoload 'turn-off-evil-matchit-mode "evil-matchit" "\
Disable evil-matchit-mode in the current buffer.

\(fn)" nil nil)

(defvar global-evil-matchit-mode nil "\
Non-nil if Global evil-matchit mode is enabled.
See the `global-evil-matchit-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-evil-matchit-mode'.")

(custom-autoload 'global-evil-matchit-mode "evil-matchit" nil)

(autoload 'global-evil-matchit-mode "evil-matchit" "\
Toggle Evil-matchit mode in all buffers.
With prefix ARG, enable Global evil-matchit mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Evil-matchit mode is enabled in all buffers where
`turn-on-evil-matchit-mode' would do it.
See `evil-matchit-mode' for more information on Evil-matchit mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "evil-matchit-c" "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-c.el"
;;;;;;  "a15980e3b14af8bdc198cea6b270060f")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-c.el

(autoload 'evilmi-c-get-tag "evil-matchit-c" "\


\(fn)" nil nil)

(autoload 'evilmi-c-jump "evil-matchit-c" "\


\(fn RLT NUM)" nil nil)

;;;***

;;;### (autoloads nil "evil-matchit-cmake" "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-cmake.el"
;;;;;;  "bd0f2e61cd11814a6eecaa2bef1c317a")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-cmake.el

(autoload 'evilmi-cmake-get-tag "evil-matchit-cmake" "\


\(fn)" nil nil)

(autoload 'evilmi-cmake-jump "evil-matchit-cmake" "\


\(fn RLT NUM)" nil nil)

;;;***

;;;### (autoloads nil "evil-matchit-diff" "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-diff.el"
;;;;;;  "614cf46bd6dc2ec2c2d6d02d8e0d6489")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-diff.el

(autoload 'evilmi-diff-get-tag "evil-matchit-diff" "\


\(fn)" nil nil)

(autoload 'evilmi-diff-jump "evil-matchit-diff" "\


\(fn RLT NUM)" nil nil)

;;;***

;;;### (autoloads nil "evil-matchit-fortran" "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-fortran.el"
;;;;;;  "03ba2dd160515d6c26443f32c7b24607")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-fortran.el

(autoload 'evilmi-fortran-get-tag "evil-matchit-fortran" "\


\(fn)" nil nil)

(autoload 'evilmi-fortran-jump "evil-matchit-fortran" "\


\(fn RLT NUM)" nil nil)

;;;***

;;;### (autoloads nil "evil-matchit-html" "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-html.el"
;;;;;;  "3afb37edb1afb2ed2c37be1665b6f0a2")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-html.el

(autoload 'evilmi-html-get-tag "evil-matchit-html" "\


\(fn)" nil nil)

(autoload 'evilmi-html-jump "evil-matchit-html" "\


\(fn RLT NUM)" nil nil)

;;;***

;;;### (autoloads nil "evil-matchit-javascript" "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-javascript.el"
;;;;;;  "58e4e67ff0ac19955428bcb6ce52d737")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-javascript.el

(autoload 'evilmi-javascript-get-tag "evil-matchit-javascript" "\


\(fn)" nil nil)

(autoload 'evilmi-javascript-jump "evil-matchit-javascript" "\


\(fn RLT NUM)" nil nil)

;;;***

;;;### (autoloads nil "evil-matchit-latex" "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-latex.el"
;;;;;;  "2a3a4d3f031e9aca91970281bbdb41d6")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-latex.el

(autoload 'evilmi-latex-get-tag "evil-matchit-latex" "\


\(fn)" nil nil)

(autoload 'evilmi-latex-jump "evil-matchit-latex" "\


\(fn RLT NUM)" nil nil)

;;;***

;;;### (autoloads nil "evil-matchit-org" "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-org.el"
;;;;;;  "34e7ebcdc305e1ab06ab63c2d8e54b72")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-org.el

(autoload 'evilmi-org-get-tag "evil-matchit-org" "\


\(fn)" nil nil)

(autoload 'evilmi-org-jump "evil-matchit-org" "\


\(fn RLT NUM)" nil nil)

;;;***

;;;### (autoloads nil "evil-matchit-python" "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-python.el"
;;;;;;  "badb805677e76845a2fd23248cc3f848")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-python.el

(autoload 'evilmi-python-get-tag "evil-matchit-python" "\


\(fn)" nil nil)

(autoload 'evilmi-python-jump "evil-matchit-python" "\


\(fn RLT NUM)" nil nil)

;;;***

;;;### (autoloads nil "evil-matchit-ruby" "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-ruby.el"
;;;;;;  "8f023ea58a7111e0fa849376f1292dc0")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-ruby.el

(autoload 'evilmi-ruby-get-tag "evil-matchit-ruby" "\


\(fn)" nil nil)

(autoload 'evilmi-ruby-jump "evil-matchit-ruby" "\


\(fn RLT NUM)" nil nil)

;;;***

;;;### (autoloads nil "evil-matchit-script" "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-script.el"
;;;;;;  "6d8077614458d83a33b03fa07e12cd24")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-script.el

(autoload 'evilmi-script-get-tag "evil-matchit-script" "\


\(fn)" nil nil)

(autoload 'evilmi-script-jump "evil-matchit-script" "\


\(fn RLT NUM)" nil nil)

;;;***

;;;### (autoloads nil "evil-matchit-sdk" "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-sdk.el"
;;;;;;  "35f3089aef413ef27f5d1f16c3be8cee")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-sdk.el

(autoload 'evilmi-sdk-curline "evil-matchit-sdk" "\


\(fn)" nil nil)

(autoload 'evilmi-sdk-member "evil-matchit-sdk" "\
Check if KEYWORD exist in KEYWORD-LIST.

\(fn KEYWORD KEYWORD-LIST)" nil nil)

(autoload 'evilmi-sdk-get-tag-info "evil-matchit-sdk" "\
Return (row column is-function-exit-point keyword).
The row and column marked position in evilmi-mylang-match-tags
is-function-exit-point could be unknown status

\(fn KEYWORD MATCH-TAGS)" nil nil)

(autoload 'evilmi-sdk-get-tag "evil-matchit-sdk" "\
Return '(start-point ((row column is-function-exit-point keyword)).

\(fn MATCH-TAGS HOWTOS)" nil nil)

(autoload 'evilmi-sdk-jump "evil-matchit-sdk" "\


\(fn RLT NUM MATCH-TAGS HOWTOS)" nil nil)

;;;***

;;;### (autoloads nil "evil-matchit-sh" "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-sh.el"
;;;;;;  "da6f65b5f28740168dc156e7ea55c69a")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-sh.el

(autoload 'evilmi-sh-get-tag "evil-matchit-sh" "\


\(fn)" nil nil)

(autoload 'evilmi-sh-jump "evil-matchit-sh" "\


\(fn RLT NUM)" nil nil)

;;;***

;;;### (autoloads nil "evil-matchit-simple" "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-simple.el"
;;;;;;  "30bf4bc606561994bd93056b4a831448")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-simple.el

(autoload 'evilmi-simple-get-tag "evil-matchit-simple" "\


\(fn)" nil nil)

(autoload 'evilmi-simple-jump "evil-matchit-simple" "\


\(fn RLT NUM)" nil nil)

;;;***

;;;### (autoloads nil "evil-matchit-sql" "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-sql.el"
;;;;;;  "622702066a5a078b2b65ecfc2e3f6617")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-sql.el

(autoload 'evilmi-sql-get-tag "evil-matchit-sql" "\


\(fn)" nil nil)

(autoload 'evilmi-sql-jump "evil-matchit-sql" "\


\(fn RLT NUM)" nil nil)

;;;***

;;;### (autoloads nil "evil-matchit-template" "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-template.el"
;;;;;;  "6b62a4773426e318d1f4f58a1c04798e")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-template.el

(autoload 'evilmi-template-get-tag "evil-matchit-template" "\


\(fn)" nil nil)

(autoload 'evilmi-template-jump "evil-matchit-template" "\


\(fn RLT NUM)" nil nil)

;;;***

;;;### (autoloads nil "evil-matchit-verilog" "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-verilog.el"
;;;;;;  "ccd8193a0cfced481818c701024fea3e")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-verilog.el

(autoload 'evilmi-verilog-get-tag "evil-matchit-verilog" "\


\(fn)" nil nil)

(autoload 'evilmi-verilog-jump "evil-matchit-verilog" "\


\(fn ORIG-INFO NUM)" nil nil)

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-c.el"
;;;;;;  "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-cmake.el"
;;;;;;  "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-diff.el"
;;;;;;  "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-fortran.el"
;;;;;;  "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-html.el"
;;;;;;  "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-javascript.el"
;;;;;;  "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-latex.el"
;;;;;;  "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-org.el"
;;;;;;  "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-pkg.el"
;;;;;;  "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-python.el"
;;;;;;  "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-ruby.el"
;;;;;;  "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-script.el"
;;;;;;  "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-sdk.el"
;;;;;;  "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-sh.el"
;;;;;;  "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-simple.el"
;;;;;;  "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-sql.el"
;;;;;;  "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-template.el"
;;;;;;  "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit-verilog.el"
;;;;;;  "../../../../../../.emacs.d/packages/evil-matchit-2.2.1/evil-matchit.el")
;;;;;;  (22770 7715 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; evil-matchit-autoloads.el ends here
