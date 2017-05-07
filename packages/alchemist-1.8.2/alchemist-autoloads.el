;;; alchemist-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "alchemist" "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist.el"
;;;;;;  "3f63cf87023d239ab579cfa5d9c0baf0")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist.el

(autoload 'alchemist-mode "alchemist" "\
Toggle alchemist mode.

Key bindings:
\\{alchemist-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "alchemist-iex" "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-iex.el"
;;;;;;  "44cfb879276317fc693b427154e00135")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-iex.el

(defalias 'run-elixir 'alchemist-iex-run)

(autoload 'alchemist-iex-run "alchemist-iex" "\
Start an IEx process.
Show the IEx buffer if an IEx process is already run.

\(fn &optional ARG)" t nil)

(autoload 'alchemist-iex-project-run "alchemist-iex" "\
Start an IEx process with mix 'iex -S mix' in the
context of an Elixir project.
Show the IEx buffer if an IEx process is already run.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "alchemist-phoenix" "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-phoenix.el"
;;;;;;  "25da0fb5b424dded378435b4cfe27297")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-phoenix.el

(autoload 'alchemist-phoenix-project-p "alchemist-phoenix" "\
Return non-nil if `default-directory' is inside an Phoenix project.

\(fn)" nil nil)

(autoload 'alchemist-phoenix-mode "alchemist-phoenix" "\
Minor mode for Elixir Phoenix web framework projects.

The following commands are available:

\\{alchemist-phoenix-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'alchemist-phoenix-enable-mode "alchemist-phoenix" "\


\(fn)" nil nil)

(dolist (hook '(alchemist-mode-hook)) (add-hook hook 'alchemist-phoenix-enable-mode))

;;;***

;;;### (autoloads nil "alchemist-refcard" "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-refcard.el"
;;;;;;  "c1af0027fc9c216e75c26f9b3c09cdb9")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-refcard.el

(autoload 'alchemist-refcard "alchemist-refcard" "\
Generate an Alchemist refcard of all the features.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "alchemist-test-mode" "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-test-mode.el"
;;;;;;  "6b42d9c44b3c6f8b453c26a107b26165")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-test-mode.el

(autoload 'alchemist-test-mode "alchemist-test-mode" "\
Minor mode for Elixir ExUnit files.

The following commands are available:

\\{alchemist-test-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'alchemist-test-enable-mode "alchemist-test-mode" "\


\(fn)" nil nil)

(dolist (hook '(alchemist-mode-hook)) (add-hook hook 'alchemist-test-enable-mode))

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-company.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-compile.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-complete.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-eval.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-execute.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-file.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-goto.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-help.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-hex.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-hooks.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-iex.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-info.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-interact.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-key.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-macroexpand.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-message.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-mix.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-phoenix.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-pkg.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-project.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-refcard.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-report.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-scope.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-server.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-test-mode.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist-utils.el"
;;;;;;  "../../../../../../.emacs.d/packages/alchemist-1.8.2/alchemist.el")
;;;;;;  (22770 7711 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; alchemist-autoloads.el ends here
