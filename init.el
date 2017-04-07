(let ((gc-cons-threshold 339430400)  ; Temporarily raise GC thresholds
      (gc-cons-percentage 0.6)       ; and disable special I/O handlers
      (file-name-handler-alist nil)) ; to optimize startup performance.

  ;; To suppress the startup messages in the echo area, this setting
  ;; needs to occupy its own setq expression in your init file.
  ;; If your init file is byte-compiled, use the following form instead:
  ;;  (eval \\='(setq inhibit-startup-echo-area-message \"YOUR-USER-NAME\"))
  (setq inhibit-startup-echo-area-message "ivan")

  (setq inhibit-startup-screen t
        initial-scratch-message nil)

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

;; Set a GC threshold greater than the default,
;; but less than the value used during startup.
;; The aim is to strike a balance between the
;; frequency and duration of GC interruptions.
(setq gc-cons-threshold 100000000)
