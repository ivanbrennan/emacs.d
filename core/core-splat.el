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

(defvar splat-buffer-widgets '(banner shortmenu)
  "List of widgets to display in a blank splat buffer.")

(define-derived-mode splat-buffer-mode fundamental-mode
  "v0"
  "Major mode for the splat buffer.")

(defvar splat-buffer--width 0)
(defvar splat-buffer--height 0)


;;
(add-hook 'emacs-startup-hook #'splat-init)

(defun ivan-not-splat-buffer? () (not (splat-buffer?)))
(add-hook 'kill-buffer-query-functions #'ivan-not-splat-buffer?)

(defun ivan-ensure-splat-reload-on-window-config-change ()
  (add-hook 'window-configuration-change-hook #'splat-buffer-reload)
  (splat-buffer-reload))
(add-hook 'window-setup-hook #'ivan-ensure-splat-reload-on-window-config-change)


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
                              'face 'font-lock-string-face) " "))
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

(defun splat-buffer-widget-shortmenu ()
  (let ((all-the-icons-scale-factor 1.3)
        (all-the-icons-default-adjust -0.05)
        (start (point))
        (sep "   ")
        (last-session-p (and (featurep 'workgroups2)
                             (f-exists-p wg-session-file)))
        end)
    (unless last-session-p
      (setq sep "     "))
    (insert
     (s-center (- splat-buffer--width 5)
               (with-temp-buffer
                 (insert-text-button
                  (concat (all-the-icons-octicon
                           "mark-github"
                           :face 'font-lock-variable-name-face)
                          (propertize " Homepage" 'face 'font-lock-variable-name-face))
                  'action '(lambda (_) (browse-url "https://github.com/ivanbrennan/emacs.d"))
                  'follow-link t)

                 (insert sep " ")

                 (insert-text-button
                  (concat (all-the-icons-octicon
                           "file-text"
                           :face 'font-lock-variable-name-face)
                          (propertize " Recent files" 'face 'font-lock-variable-name-face))
                  'action '(lambda (_) (call-interactively 'ivan-recentf))
                  'follow-link t)

                 (insert sep)

                 (insert-text-button
                  (concat (all-the-icons-octicon
                           "tools"
                           :face 'font-lock-variable-name-face)
                          (propertize " Edit emacs.d" 'face 'font-lock-variable-name-face))
                  'action '(lambda (_) (find-file (f-expand "init.el" user-emacs-directory)))
                  'follow-link t)

                 (when last-session-p
                   (insert sep)

                   (insert-text-button
                    (concat (all-the-icons-octicon
                             "history"
                             :face 'font-lock-variable-name-face)
                            (propertize " Reload last session" 'face 'font-lock-variable-name-face))
                    'action '(lambda (_) (doom:workgroup-load))
                    'follow-link t))

                 (setq end (point))
                 (buffer-string))))))

(defun doom:workgroup-load (&optional bang session-name)
  (doom|wg-cleanup)
  (wg-open-session wg-session-file)
  ;; (doom/workgroup-display t)
  )

;; (defun doom/workgroup-display (&optional suppress-update return-p message)
;;   (interactive)
;;   (awhen (wg-current-session t)
;;     (unless (eq suppress-update t)
;;       (doom/workgroup-update-names (if (wg-workgroup-p suppress-update) suppress-update)))
;;     (let ((output (wg-display-internal
;;                    (lambda (workgroup index)
;;                      (if (not workgroup) wg-nowg-string
;;                        (wg-element-display
;;                         workgroup
;;                         (format " [%d] %s " (1+ index) (wg-workgroup-name workgroup))
;;                         'wg-current-workgroup-p)))
;;                    (wg-session-workgroup-list it))))
;;       (if return-p
;;           output
;;         (message "%s%s" output (or message ""))))))

(provide 'core-splat)
;;; core-splat.el ends here
