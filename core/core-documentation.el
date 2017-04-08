(global-eldoc-mode 0)
(add-hook 'emacs-lisp-mode-hook  #'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

(setq apropos-do-all     t
      help-window-select t)

(with-eval-after-load 'info
  (add-to-list 'Info-additional-directory-list
               (ivan-emacs-file "info/")))

(with-eval-after-load 'help
  (setq source-directory "~/Development/code/elisp/emacs-mac"))

(provide 'core-documentation)
