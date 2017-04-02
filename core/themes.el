(setq custom-theme-directory (ivan-emacs-file "themes/"))

(mkdir_p custom-theme-directory)

(defvar ivan-themes [elixir dome arjen-grey FlatUI chalk])
(defvar ivan-theme-index 0)
(defvar ivan-rotated-theme-hook nil
  "Hook called after the theme has been rotated")

(defsubst ivan-theme-at-index (x) (aref ivan-themes x))
(defsubst ivan-next-theme     () (interactive) (ivan-rotate-theme +1))
(defsubst ivan-previous-theme () (interactive) (ivan-rotate-theme -1))
(defsubst ivan-disable-themes () (mapc #'disable-theme custom-enabled-themes))

(defun ivan-rotate-theme (inc)
  (let* ((index (mod (+ inc ivan-theme-index) (length ivan-themes)))
         (theme (ivan-theme-at-index index)))
    (when (ivan-load-theme theme)
      (setq ivan-theme-index index)
      (run-hooks 'ivan-rotated-theme-hook)
      (message (symbol-name theme)))))

(defun ivan-load-theme (theme)
  (let ((backup (ivan-disable-themes)))
    (condition-case nil
        (load-theme theme 'no-confirm)
      (error
       (ivan-restore-themes backup)
       nil))))

(defun ivan-restore-themes (backup)
  (ivan-disable-themes)
  (mapc #'ivan-load-theme (reverse backup)))

(ivan-load-theme (ivan-theme-at-index ivan-theme-index))
