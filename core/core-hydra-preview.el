(with-eval-after-load 'hydra
  (defhydra hydra-preview (:hint nil
                           :pre        (setq hydra-lv nil)
                           :after-exit (setq hydra-lv t))
    (concat) ; empty doc-string
    ("SPC"      dired-preview-current)
    ("n"        dired-preview-next)
    ("p"        dired-preview-previous)
    ("j"        dired-next-line)
    ("C-n"      dired-next-line)
    ("k"        dired-previous-line)
    ("C-p"      dired-previous-line)
    ("q"        dired-preview-quit :exit t)
    ("<escape>" nil :exit t)))

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
  (dired-preview--assert-mode)
  (let ((win (dired-preview--find-window 'or-create))
        (file (dired-get-file-for-visit)))
    (dired-preview--maybe-kill-window-buffer win)
    (with-selected-window win
      (dired-preview--view-file file))))

(defun dired-preview-quit ()
  "Quit previewing from Dired, clean up preview buffer and window."
  (interactive)
  (dired-preview--assert-mode)
  (let ((win (dired-preview--find-window)))
    (dired-preview--maybe-kill-window-buffer win)
    (dired-preview--maybe-delete-window win)))

(defun ivan-view-quit ()
  "Quit view-mode, clean up preview buffer and window."
  (interactive)
  (let ((win (selected-window)))
    (dired-preview--maybe-kill-window-buffer win 'can-kill-current)
    (dired-preview--maybe-delete-window win)))

(defun dired-preview--assert-mode ()
  (unless (eq 'dired-mode major-mode)
    (user-error "dired-preview is made for use with dired")))

(defun dired-preview--find-window (&optional or-create)
  (if (window-parent)
      (next-window)
    (when or-create (dired-preview--create-window))))

(defun dired-preview--create-window ()
  (let ((win (split-window-sensibly)))
    (unless win (error "Failed to create preview window"))
    (set-window-parameter win 'created-for-preview t)
    win))

(defun dired-preview--maybe-kill-window-buffer (win &optional can-kill-current)
  (let ((buf (window-buffer win)))
    (when (dired-preview--can-kill-buffer? buf can-kill-current)
      (kill-buffer buf))))

(defun dired-preview--can-kill-buffer? (buf can-kill-current)
  (and (or can-kill-current (not (eq (current-buffer) buf)))
       (not (buffer-modified-p buf))
       (or (buffer-local-value 'view-mode buf)
           (eq 'dired-mode (buffer-local-value 'major-mode buf)))))

(defun dired-preview--maybe-delete-window (win)
  (when (window-parameter win 'created-for-preview)
    (delete-window win)))

(defun dired-preview--view-file (file)
  (let ((buf (get-file-buffer file)))
    (if buf
        (switch-to-buffer buf)
      (let ((inhibit-message t))
        (view-file file)))))

(with-eval-after-load 'view
  (define-key view-mode-map "q" #'ivan-view-quit))

(provide 'core-hydra-preview)
