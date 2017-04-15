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

(custom-set-variables
 `(auto-save-file-name-transforms '((".*" ,ivan-auto-save-directory 'uniquify)))
 `(auto-save-list-file-prefix     ,ivan-auto-save-list-file-prefix)
 '(backup-by-copying              t)
 `(backup-directory-alist         '(("." . ,ivan-backup-directory)))
 `(ido-save-directory-list-file   ,ivan-ido-persistency-file)
 `(save-place-file                ,ivan-save-place-file)
 '(savehist-additional-variables  '(extended-command-history
                                    global-mark-ring
                                    mark-ring
                                    read-expression-history
                                    regexp-search-ring
                                    search-ring))
 '(savehist-autosave-interval     60)
 `(savehist-file                  ,ivan-savehist-file)
 `(tramp-persistency-file-name    ,ivan-tramp-persistency-file))

(use-package saveplace
  :init
  (save-place-mode +1))

(use-package savehist
  :init
  (savehist-mode +1)
  :config
  (defun unpropertize-element (e)
    (if (stringp e) (substring-no-properties e) e))
  (defun unpropertize-list-var (v)
    (when (boundp v)
      (set v (mapcar #'unpropertize-element (symbol-value v)))))
  (defun unpropertize-savehist ()
    (mapc #'unpropertize-list-var (append savehist-minibuffer-history-variables
                                          savehist-additional-variables)))
  (add-hook 'kill-emacs-hook    #'unpropertize-savehist)
  (add-hook 'savehist-save-hook #'unpropertize-savehist))

(provide 'core-persistence)
