;; mouse integration
(require 'mouse)
(xterm-mouse-mode t)
(global-set-key [mouse-4] '(lambda ()
			     (interactive)
			     (scroll-down 1)))
(global-set-key [mouse-5] '(lambda ()
			     (interactive)
			     (scroll-up 1)))
(setq mouse-sel-mode t)
(defun track-mouse (e))

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
(setq scroll-step 1
      scroll-conservatively 200
      mouse-wheel-scroll-amount '(1 ((shift) . 5)))

;; Fringes
(fringe-mode '(4 . 0))
