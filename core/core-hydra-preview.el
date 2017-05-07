(with-eval-after-load 'hydra
  (defhydra hydra-preview (:hint nil
                           :pre        (setq hydra-lv nil)
                           :after-exit (setq hydra-lv t))
    (format "")
    ("SPC"      dired-preview-current)
    ("n"        dired-preview-next)
    ("p"        dired-preview-previous)
    ("j"        dired-next-line)
    ("C-n"      dired-next-line)
    ("k"        dired-previous-line)
    ("C-p"      dired-previous-line)
    ("q"        dired-preview-quit :color blue)
    ("<escape>" dired-preview-quit :color blue)))

(defun dired-preview-next (&optional count)
  "Move down lines and preview dired entry."
  (interactive)
  (and (dired-next-line (or count 1))
       (dired-preview-current)))

(defun dired-preview-previous (&optional count)
  "Move up lines and preview dired entry."
  (interactive)
  (dired-preview-next (- (or count 1))))

(defun dired-preview-current ()
  "Preview the current file in another window."
  (interactive)
  (dired-preview-assert-mode)
  (let ((win  (dired-preview-find-window 'or-create))
        (file (dired-get-file-for-visit)))
    (dired-preview-clean win :keep-buffer (current-buffer))
    (with-selected-window win
      (dired-preview-file file))))

(defun dired-preview-assert-mode ()
  (unless (eq 'dired-mode major-mode)
    (user-error "dired-preview is designed for use from dired")))

(defun dired-preview-find-window (&optional or-create)
  (if (window-parent)
      (next-window)
    (when or-create (dired-preview-create-window))))

(defun dired-preview-create-window ()
  (let ((win (split-window-sensibly)))
    (unless win (error "Failed to create preview window"))
    (set-window-parameter win 'created-for-preview t)
    win))

(defun dired-preview-clean (win &rest args)
  (let ((quit-preview (plist-get args :quit-preview))
        (keep-buffer (plist-get args :keep-buffer))
        (buf (window-buffer win)))
    (when (and (not (eq keep-buffer buf))
               (dired-preview-disposable? buf))
      (kill-buffer buf)
      (when (and quit-preview
                 (window-parameter win 'created-for-preview))
        (delete-window win)))))

(defun dired-preview-disposable? (buf)
  (and (not (buffer-modified-p buf))
       (or (buffer-local-value 'view-mode buf)
           (eq 'dired-mode (buffer-local-value 'major-mode buf)))))

(defun dired-preview-created-window? (win)
  (window-parameter win 'created-for-preview))

(defun dired-preview-file (file)
  (let ((buf (get-file-buffer file)))
    (if buf
        (switch-to-buffer buf)
      (let ((inhibit-message t))
        (view-file file)))))

(defun dired-preview-quit ()
  "Quit the preview buffer, and possibly its window, from dired."
  (interactive)
  (dired-preview-assert-mode)
  (let ((win (dired-preview-find-window)))
    (dired-preview-clean win :keep-buffer (current-buffer)
                             :quit-preview t)))

(defun ivan-view-quit ()
  "Quit view-mode and clean up buffer/window."
  (interactive)
  (dired-preview-clean (selected-window) :quit-preview t))

(with-eval-after-load 'view
  (define-key view-mode-map "q" #'ivan-view-quit))

(provide 'core-hydra-preview)
