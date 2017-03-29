;;; core-splat.el

(setq initial-major-mode 'splat-buffer-mode)

(defvar splat-buffer nil
  "A global and persistent scratch buffer.")

(defvar splat-buffer-name "*splat*"
  "The name of the splat buffer.")

(defvar splat-buffer-edited? nil
  "If non-nil, the splat buffer has been edited.")

(defvar splat-buffer-inhibit-refresh nil
  "If non-nil, the splat buffer won't be refreshed.")

(defvar splat-buffer-modeline (doom-modeline 'scratch)
  "Modeline format for splat buffer.")

(defvar splat-buffer-widgets '(banner)
  "List of widgets to display in a blank splat buffer.")

(define-derived-mode splat-buffer-mode fundamental-mode
  "v1"
  "Major mode for the splat buffer.")

(defvar splat-buffer--width 0)
(defvar splat-buffer--height 0)


;;
(add-hook 'emacs-startup-hook #'splat-init)

(defun ivan/not-splat-buffer? () (not (splat-buffer?)))
(add-hook 'kill-buffer-query-functions #'ivan/not-splat-buffer?)

(defun ivan/ensure-splat-reload-on-window-config-change ()
  (add-hook 'window-configuration-change-hook #'splat-buffer-reload)
  (splat-buffer-reload))
(add-hook 'window-setup-hook #'ivan/ensure-splat-reload-on-window-config-change)


;;
(defun splat-buffer? (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (eq buffer splat-buffer))))

(defun ensure-splat-buffer ()
  "Ensure the splat buffer exists and is alive (otherwise create it)."
  ;; Rename the old scratch buffer, if it exists.
  (let ((old-scratch (get-buffer "*scratch*")))
    (when old-scratch (kill-buffer old-scratch)))
  ;; Ensure the splat buffer is alive!
  (unless (buffer-live-p splat-buffer)
    (setq splat-buffer nil))
  (unless splat-buffer
    (setq splat-buffer (get-buffer-create splat-buffer-name)))
  splat-buffer)

(defun splat-init ()
  (interactive)
  (splat-buffer-reload)
  (switch-to-splat-buffer)
  nil)

(defun switch-to-splat-buffer ()
  (interactive)
  (switch-to-buffer splat-buffer))

(defun splat-buffer-force-reload ()
  (setq splat-buffer-edited? nil)
  (splat-buffer-reload))

(defun splat-clear-on-insert ()
  "Erase the buffer and prepare it to be used like a normal buffer."
  (erase-buffer)
  ;; (set-window-margins (get-buffer-window splat-buffer) 0 0)
  (setq splat-buffer-edited? t
        mode-line-format (doom-modeline))
  (remove-hook 'evil-insert-state-entry-hook 'splat-clear-on-insert t))

(defun splat-buffer-reload (&optional dir)
  "Update the splat buffer (or create it, if it doesn't exist)."
  (when (and (not splat-buffer-inhibit-refresh)
             (not (minibuffer-window-active-p (minibuffer-window)))
             (get-buffer-window-list splat-buffer nil t)
             (or (not splat-buffer-edited?) dir))
    (let ((old-pwd (or dir default-directory)))
      (with-current-buffer (ensure-splat-buffer)
        (splat-buffer-mode)
        (add-hook 'evil-insert-state-entry-hook 'splat-clear-on-insert nil t)
        (add-hook 'after-change-major-mode-hook 'splat-clear-on-insert nil t)
        (setq splat-buffer-edited? nil)

        (erase-buffer)
        (let ((splat-buffer--width (1- (window-width (get-buffer-window splat-buffer))))
              (splat-buffer--height (window-height (get-buffer-window splat-buffer))))
          (insert (make-string (max 0 (- (truncate (/ splat-buffer--height 2)) 12)) ?\n))
          (mapc (lambda (widget-name)
                  (funcall (intern (format "splat-buffer-widget-%s" widget-name)))
                  (insert "\n\n"))
                splat-buffer-widgets))

        (setq default-directory old-pwd)
        (setq mode-line-format (doom-modeline 'scratch)))))
  t)

(defun splat-buffer-widget-banner ()
  (mapc (lambda (line)
          (insert "\n")
          (insert (propertize (s-center splat-buffer--width line)
                              'face 'font-lock-comment-face) " "))
        '("+++++++                                    +++++++"
          "o+++++o              +++++++o+             +++++++"
          "o++++o             ++++++++++++             +++++o"
          "o++++             ++++++++++++++             +++++"
          "++++              ++o+++++++++++              +++o"
          "o+++                                          ++++"
          "o++                                           ++++"
          "o++               ++++++++++++++++++++++++++++++++"
          "o++               ++++++++++++++++++++++++++++++++"
          "o++               ++++++++++++++++++++++++++++++++"
          "o++               o+++++++++++++++++++++++++++++++"
          "++++               +++++++++++++++++++++o+++++++++"
          "o++++              +++++++++++++++++++++   +o+++++"
          "++++++              ++++++++++++++++++        ++++"
          "o++++++               ++++++++++++o+         +++++"
          "o+++++++                 +++++++           +++++++"
          "++++++++                                   +++++++")))

(provide 'core-splat)
;;; core-splat.el ends here
