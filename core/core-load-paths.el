(defconst ivan-core-directory
  (eval-when-compile (expand-file-name "core" user-emacs-directory))
  "Directory for core configuration files.")

(defconst ivan-cache-directory
  (eval-when-compile (expand-file-name ".cache" user-emacs-directory))
  "Storage area for persistent files.")

(defconst ivan-packages-directory
  (eval-when-compile (expand-file-name "packages" user-emacs-directory))
  "Directory for packages.")

(defconst ivan-config-directory
  (eval-when-compile (expand-file-name "config" user-emacs-directory))
  "Directory for feature configuration files.")

(defsubst ivan-emacs-file  (x) (expand-file-name x user-emacs-directory))
(defsubst ivan-cache-file  (x) (expand-file-name x ivan-cache-directory))
(defsubst add-to-load-path (x) (add-to-list 'load-path x))
(defsubst mkdir_p          (x) (make-directory x 'mkdir_p))

(setq custom-file (eval-when-compile (ivan-emacs-file "custom.el")))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(mapc #'mkdir_p (eval-when-compile `(,ivan-cache-directory
                                     ,ivan-packages-directory)))

(mapc #'add-to-load-path (eval-when-compile `(,ivan-config-directory
                                              ,ivan-core-directory)))
