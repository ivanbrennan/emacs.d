(let ((gc-cons-threshold 339430400)  ; Temporarily raise GC thresholds
      (gc-cons-percentage 0.6)       ; and disable special I/O handlers
      (file-name-handler-alist nil)) ; to optimize startup performance.

  (setq inhibit-startup-echo-area-message "ivan")

  (load (concat user-emacs-directory "core/core-load-paths") nil 'nomessage)
  (require 'core-package)
  (require 'core-persistence)
  (require 'core-ui)
  (require 'core-parens)
  (require 'core-syntax)
  (require 'core-window)
  (require 'core-eval)
  (require 'core-editor)
  (require 'core-scroll)
  (require 'core-whitespace)
  (require 'core-themes)
  (require 'core-documentation)
  (load (concat user-emacs-directory "core") nil 'nomessage)

  (and (display-graphic-p)
       (require 'server)
       (or (server-running-p)
           (server-start))))

;; Set a GC threshold greater than the default,
;; but less than the value used during startup.
;; The aim is to strike a balance between the
;; frequency and duration of GC interruptions.
(setq gc-cons-threshold 100000000)
