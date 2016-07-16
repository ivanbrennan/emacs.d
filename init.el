;; auto-save in subdirectory of emacs
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save/") t)))

;; backups in subdirectory of emacs
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                  (concat user-emacs-directory "backups")))))
(setq backup-by-copying t)

;; scratch
(setq initial-scratch-message "")

;; savehist
(savehist-mode 1)

;; hide the toolbar
(tool-bar-mode -1)

;; minibuffer
(setq minibuffer-eldef-shorten-default t)
(minibuffer-electric-default-mode)

;; no startup screen
(setq inhibit-startup-screen t)

;; cursor
(setq-default cursor-type 'bar)
(set-cursor-color "#f70000")

;; whitespace
(setq-default show-trailing-whitespace 1)

;; logical lines
(setq-default truncate-lines t)

;; mouse
(require 'mouse)
(xterm-mouse-mode t)
(setq mouse-sel-mode t)
;;(mac-mouse-wheel-mode 0)
(defun track-mouse (e))
;; the following scroll events are overridden
;; by mac-win.el in the emacs-mac-port distro
(global-set-key [wheel-up] (lambda ()
			     (interactive)
			     (scroll-down 1)))
(global-set-key [wheel-down] (lambda ()
			       (interactive)
			       (scroll-up 1)))
(global-set-key [wheel-right] (lambda ()
				(interactive)
				(scroll-left 1)))
(global-set-key [wheel-left] (lambda ()
			       (interactive)
			       (scroll-right 1)))

;; Default font face
(set-face-attribute 'default t :font "Source Code Pro-14")
(set-frame-font "Source Code Pro-14" nil t)

;; smooth scrolling
(setq scroll-step 1
      scroll-conservatively 200
      mouse-wheel-scroll-amount '(0.01 ((shift) . 1)))

;; fringes
(fringe-mode '(4 . 1))

(setq-default indicate-empty-lines t)
