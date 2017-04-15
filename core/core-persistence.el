(defconst ivan-auto-save-directory
  (eval-when-compile (ivan-cache-file "auto-save"))
  "Storage area for auto-save files.")

(defconst ivan-auto-save-list-file-prefix
  (eval-when-compile (ivan-cache-file "auto-save-list/.saves-"))
  "Prefix for generating `auto-save-list-file-name'.")

(defconst ivan-backup-directory
  (eval-when-compile (ivan-cache-file "backups/"))
  "Storage area for backup files.")

(defconst ivan-ido-persistency-file
  (eval-when-compile (ivan-cache-file "ido.last"))
  "File where ido state is persisted.")

(defconst ivan-save-place-file
  (eval-when-compile (ivan-cache-file "saveplace"))
  "File where save-place-alist is persisted.")

(defconst ivan-savehist-file
  (eval-when-compile (ivan-cache-file "savehist"))
  "File where minibuffer history is persisted.")

(defconst ivan-tramp-persistency-file
  (eval-when-compile (ivan-cache-file "tramp"))
  "File which keeps connection history for Tramp connections.")

(mkdir_p ivan-auto-save-directory)

(setq auto-save-file-name-transforms `((".*" ,ivan-auto-save-directory 'uniquify))
      auto-save-list-file-prefix     ivan-auto-save-list-file-prefix

      backup-by-copying      t
      backup-directory-alist `(("." . ,ivan-backup-directory))

      ido-save-directory-list-file ivan-ido-persistency-file)

(with-eval-after-load 'tramp-cache
  (eval-when-compile (defvar tramp-persistency-file-name))
  (setq tramp-persistency-file-name ivan-tramp-persistency-file))

(use-package saveplace
  :config
  (setq save-place-file ivan-save-place-file)
  (save-place-mode +1))

(use-package savehist
  :config
  (setq savehist-file                 ivan-savehist-file
        savehist-autosave-interval    60
        savehist-additional-variables '(extended-command-history
                                        global-mark-ring
                                        mark-ring
                                        read-expression-history
                                        regexp-search-ring
                                        search-ring))
  (defun unpropertize-savehist ()
    (mapc #'unpropertize-list-var (append savehist-minibuffer-history-variables
                                          savehist-additional-variables)))
  (defun unpropertize-list-var (v)
    (when (boundp v)
      (set v (mapcar #'unpropertize-element (symbol-value v)))))
  (defun unpropertize-element (e)
    (if (stringp e) (substring-no-properties e) e))
  (add-hook 'savehist-save-hook #'unpropertize-savehist)
  (add-hook 'kill-emacs-hook    #'unpropertize-savehist)
  (savehist-mode +1))

(provide 'core-persistence)
