(defconst ivan-core-directory
  (expand-file-name "core" user-emacs-directory)
  "Directory for core configuration files.")
(defconst ivan-cache-directory
  (expand-file-name ".cache" user-emacs-directory)
  "Storage area for persistent files.")
(defconst ivan-packages-directory
  (expand-file-name "packages" user-emacs-directory)
  "Directory for packages.")
(defconst ivan-config-directory
  (expand-file-name "config" user-emacs-directory)
  "Directory for feature configuration files.")

(defsubst ivan-emacs-file  (x) (expand-file-name x user-emacs-directory))
(defsubst ivan-cache-file  (x) (expand-file-name x ivan-cache-directory))
(defsubst add-to-load-path (x) (add-to-list 'load-path x))
(defsubst mkdir_p          (x) (make-directory x 'mkdir_p))

(setq custom-file (ivan-emacs-file "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(mkdir_p ivan-cache-directory)

(mapc #'add-to-load-path `(,ivan-core-directory
                           ,ivan-config-directory))
