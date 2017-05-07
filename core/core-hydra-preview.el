(with-eval-after-load 'hydra
  (defhydra hydra-dired-preview (:hint nil)
    (format (propertize "preview" 'face 'hydra-face-title))
    ("SPC"      dired-preview-current)
    ("S-SPC"    dired-preview-current)
    ("j"        dired-next-line)
    ("n"        dired-next-line)
    ("C-n"      dired-next-line)
    ("k"        dired-previous-line)
    ("p"        dired-previous-line)
    ("C-p"      dired-previous-line)
    ("."        hydra-dired-preview-and-follow :exit t)
    ("q"        dired-quit-preview :color blue)
    ("<escape>" dired-quit-preview :color blue))

  (defhydra hydra-dired-follow (:hint nil)
    (format (propertize "preview (follow)" 'face 'hydra-face-title))
    ("j"        dired-preview-next)
    ("n"        dired-preview-next)
    ("C-n"      dired-preview-next)
    ("k"        dired-preview-previous)
    ("p"        dired-preview-previous)
    ("C-p"      dired-preview-previous)
    ("."        hydra-dired-preview/body :exit t)
    ("q"        dired-quit-preview :color blue)
    ("<escape>" dired-quit-preview :color blue)))

(defun hydra-dired-preview-and-follow ()
  (interactive)
  (dired-preview-current)
  (hydra-dired-follow/body))

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
    (dired-preview-clean win)
    (with-selected-window win
      (dired-preview-file file))))

(defun dired-preview-file (file)
  (let ((buf (get-file-buffer file)))
    (if buf
        (switch-to-buffer buf)
      (let ((inhibit-message t))
        (view-file file)))))

(defun dired-quit-preview ()
  "Quit the preview buffer, and possibly its window, from dired."
  (interactive)
  (dired-preview-assert-mode)
  (let ((win (dired-preview-find-window)))
    (dired-preview-clean win 'quit-preview)))

(defun dired-preview-find-window (&optional or-create)
  (if (window-parent)
      (next-window)
    (when or-create
      (dired-preview-create-window))))

(defun dired-preview-create-window ()
  (let ((win (split-window-sensibly)))
    (unless win (error "Failed to create preview window"))
    (set-window-parameter win 'created-for-preview t)
    win))

(defun dired-preview-created-window? (win)
  (window-parameter win 'created-for-preview))

(defun dired-preview-clean (win &optional quit-preview)
  (let ((buf (window-buffer win)))
    (when (and (not (eq (current-buffer) buf))
               (dired-preview-disposable? buf))
      (kill-buffer buf)
      (when (and quit-preview
                 (window-parameter win 'created-for-preview))
        (delete-window win)))))

(defun dired-preview-disposable? (buf)
  (and (not (buffer-modified-p buf))
       (or (buffer-local-value 'view-mode buf)
           (eq 'dired-mode
               (buffer-local-value 'major-mode buf)))))

(defun dired-preview-assert-mode ()
  (unless (derived-mode-p 'dired-mode)
    (user-error "Preview is designed for use from dired")))

(provide 'core-hydra-preview)
