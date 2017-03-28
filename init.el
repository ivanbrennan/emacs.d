(let ((gc-cons-threshold 339430400)
      (gc-cons-percentage 0.6)
      (file-name-handler-alist nil))

  (setq inhibit-startup-echo-area-message "ivan")
  ;; If your init file is byte-compiled, use the following form instead:
  ;;  (eval \\='(setq inhibit-startup-echo-area-message \"YOUR-USER-NAME\"))

  (load-file (concat user-emacs-directory "core.el"))

  (when (display-graphic-p)
    (require 'server)
    (unless (server-running-p)
      (server-start))))
