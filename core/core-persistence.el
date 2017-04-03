(defconst ivan-auto-save-directory
  (expand-file-name "auto-save" ivan-cache-directory)
  "Storage area for auto-save files.")

(mkdir_p ivan-auto-save-directory)

(setq
 auto-save-file-name-transforms `((".*" ,ivan-auto-save-directory 'uniquify))
 auto-save-list-file-prefix      (ivan-cache-file "auto-save-list/.saves-")
 backup-by-copying               t
 backup-directory-alist         `(("." . ,(ivan-cache-file "backups/")))
 ido-save-directory-list-file    (ivan-cache-file "ido.last")
 tramp-persistency-file-name     (ivan-cache-file "tramp"))

(provide 'core-persistence)
