(let ((gc-cons-threshold 339430400)
      (gc-cons-percentage 0.6)
      (file-name-handler-alist nil))

  ;; blank slate
  (setq inhibit-startup-echo-area-message "ivan")
  ;; If your init file is byte-compiled, use the following form instead:
  ;;  (eval \\='(setq inhibit-startup-echo-area-message \"YOUR-USER-NAME\"))
  (setq inhibit-startup-screen t
        initial-scratch-message nil)

  ;; clean screen
  (menu-bar-mode   0)
  (tool-bar-mode   0)
  (when (display-graphic-p)
    (scroll-bar-mode 0)
    (tooltip-mode    0)
    (setq frame-title-format "%b"))

  (load-file (concat user-emacs-directory "core/core-load-paths.el"))
  (require 'core-persistence)
  (require 'core-themes)
  (load-file (concat user-emacs-directory "core.el"))

  (when (display-graphic-p)
    (require 'server)
    (unless (server-running-p)
      (server-start))))

(setq gc-cons-threshold 100000000)
