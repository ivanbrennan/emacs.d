(defconst ivan-auto-save-directory
  (eval-when-compile (ivan-cache-file "auto-save"))
  "Storage area for auto-save files.")

(mkdir_p ivan-auto-save-directory)

(setq
 auto-save-file-name-transforms `((".*" ,ivan-auto-save-directory 'uniquify))
 auto-save-list-file-prefix     (ivan-cache-file "auto-save-list/.saves-")
 backup-by-copying              t
 backup-directory-alist         `(("." . ,(ivan-cache-file "backups/")))
 ido-save-directory-list-file   (ivan-cache-file "ido.last")
 tramp-persistency-file-name    (ivan-cache-file "tramp"))

(use-package savehist
  :init
  (setq
   savehist-file                 (ivan-cache-file "savehist")
   savehist-autosave-interval    60
   savehist-additional-variables '(extended-command-history
                                   global-mark-ring
                                   mark-ring
                                   read-expression-history
                                   regexp-search-ring
                                   search-ring))
  (savehist-mode +1)
  :config
  (defun unpropertize-list-var (v)
    (when (boundp v)
      (set v (mapcar #'substring-no-properties (symbol-value v)))))
  (defun unpropertize-savehist ()
    (mapc #'unpropertize-list-var (append savehist-minibuffer-history-variables
                                          savehist-additional-variables)))
  (add-hook 'kill-emacs-hook    'unpropertize-savehist)
  (add-hook 'savehist-save-hook 'unpropertize-savehist))

(use-package saveplace
  :init
  (save-place-mode +1)
  (setq save-place-file (ivan-cache-file "saveplace")))

(provide 'core-persistence)
