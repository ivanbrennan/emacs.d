(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(put 'dired-find-file :advertised-binding "f")

(provide 'dired-config)
