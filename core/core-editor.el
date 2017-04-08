(prefer-coding-system 'utf-8)

(setq-default bidi-display-reordering nil)

(add-hook 'help-mode-hook #'variable-pitch-mode)
(add-hook 'Info-mode-hook #'variable-pitch-mode)

(defun ivan-truncate-lines () (setq truncate-lines t))
(defun ivan-wrap-lines     () (setq truncate-lines nil))

(dolist (hook '(prog-mode-hook
                compilation-mode-hook
                occur-mode-hook
                dired-mode-hook))
  (add-hook hook #'ivan-truncate-lines))

(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'help-mode-hook #'visual-line-mode)

(with-current-buffer "*Messages*"
  (visual-line-mode +1))

(setq-default fill-column           80
              require-final-newline t)

(provide 'core-editor)
