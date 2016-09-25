(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(put 'dired-find-file :advertised-binding "f")
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(provide 'dired-config)
