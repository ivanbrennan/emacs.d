(let ((gc-cons-threshold 402653184)  ; Temporarily raise GC thresholds
      (gc-cons-percentage 0.6)       ; and disable special I/O handlers
      (file-name-handler-alist nil)) ; to optimize startup performance.

  (setq inhibit-startup-echo-area-message "ivan")

  (require 'core-load-paths (concat user-emacs-directory "core/core-load-paths"))
  (require 'core-emacs)
  (require 'core-package)
  (require 'core-persistence)
  (require 'core-ui)
  (require 'core-modeline)
  (require 'core-splat)
  (require 'core-parens)
  (require 'core-syntax)
  (require 'core-window)
  (require 'core-eval)
  (require 'core-editor)
  (require 'core-scroll)
  (require 'core-whitespace)
  (require 'core-themes)
  (require 'core-documentation)
  (require 'core-hidden-mode-line)
  (require 'core (concat user-emacs-directory "core"))

  (and (display-graphic-p)
       (require 'server)
       (or (server-running-p)
           (server-start))))

;; Set a GC threshold greater than the default,
;; but less than the value used during startup.
;; The aim is to strike a balance between the
;; frequency and duration of GC interruptions.
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)
