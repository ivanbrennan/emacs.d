(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(put 'dired-find-file :advertised-binding "f")

(defun ivan/up-directory ()
  (interactive)
  (find-alternate-file ".."))

(define-key dired-mode-map "^" 'ivan/up-directory)
(evil-define-key 'normal dired-mode-map "_" 'ivan/up-directory)

(provide 'dired-config)
