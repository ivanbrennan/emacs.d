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

(define-inline ivan-emacs-file (x)
  (inline-letevals (x)
    (inline-quote (expand-file-name ,x user-emacs-directory))))

(define-inline ivan-cache-file (x)
  (inline-letevals (x)
    (inline-quote (expand-file-name ,x ivan-cache-directory))))

(defun add-to-load-path (x) (add-to-list 'load-path x))
(defun mkdir_p          (x) (make-directory x 'mkdir_p))

(let ((file (eval-when-compile (ivan-emacs-file "custom.el"))))
  (unless (file-exists-p file) (write-region "" nil file))
  (custom-set-variables `(custom-file ,file)))

(mapc #'mkdir_p (eval-when-compile `(,ivan-cache-directory
                                     ,ivan-packages-directory)))

(mapc #'add-to-load-path (eval-when-compile `(,ivan-config-directory
                                              ,ivan-core-directory)))

(provide 'core-load-paths)
