(let ((gc-cons-threshold 339430400)  ; Temporarily raise GC thresholds
      (gc-cons-percentage 0.6)       ; and disable special I/O handlers
      (file-name-handler-alist nil)) ; to optimize startup performance.

  (setq inhibit-startup-echo-area-message "ivan")

  (load-file (concat user-emacs-directory "core/core-load-paths.el"))

  (require 'core-persistence)
  (require 'core-ui)
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
