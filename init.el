;; Auto-save in subdirectory of emacs
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save/") t)))

;; Backups in subdirectory of emacs
(setq backup-directory-alist
      `(("." . ,(expand-file-name
		 (concat user-emacs-directory "backups")))))

;; Hide the toolbar
(tool-bar-mode -1)

;; Default font face
(set-face-attribute 'default t :font "Source Code Pro-14")
(set-frame-font "Source Code Pro-14" nil t)

;; Smooth scrolling
(setq scroll-step           1
      scroll-conservatively 200
      auto-window-vscroll   nil)
