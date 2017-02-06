(let ((gc-cons-threshold 339430400)
      (gc-cons-percentage 0.6)
      (file-name-handler-alist nil))

  (load-file (concat user-emacs-directory "core.el"))

  (when (display-graphic-p)
    (require 'server)
    (unless (server-running-p)
      (server-start))))
