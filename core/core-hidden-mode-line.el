(put 'hidden-mode-line-mode 'permanent-local t)
(put 'hidden-mode-line 'permanent-local t)

(defvar hidden-mode-line-format nil
  "Format to use when `hidden-mode-line-mode' replaces the mode-line")

(defvar-local hidden-mode-line nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  (if hidden-mode-line-mode
      (setq hidden-mode-line mode-line-format
            mode-line-format hidden-mode-line-format)
    (setq mode-line-format hidden-mode-line
          hidden-mode-line hidden-mode-line-format)))

(with-current-buffer "*Messages*" (hidden-mode-line-mode +1))
(dolist (hook '(help-mode-hook
                apropos-mode-hook
                ibuffer-mode-hook
                compilation-mode-hook
                messages-buffer-mode-hook
                completion-list-mode-hook))
  (add-hook hook #'hidden-mode-line-mode))

(provide 'core-hidden-mode-line)
