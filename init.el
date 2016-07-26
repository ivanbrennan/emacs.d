;; packages
(require 'package)

(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "https://marmalade-repo.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(setq package-enable-at-startup nil)

(package-initialize)

(defconst ivan-cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "Storage area for persistent files")
(unless (file-exists-p ivan-cache-directory)
  (make-directory ivan-cache-directory))

(defconst ivan-auto-save-directory
  (expand-file-name (concat ivan-cache-directory "auto-save/"))
  "Auto-save directory")
(unless (file-exists-p ivan-auto-save-directory)
  (make-directory ivan-auto-save-directory))
(setq auto-save-file-name-transforms `((".*" ,ivan-auto-save-directory t)))

(setq savehist-file (concat ivan-cache-directory "savehist"))
(savehist-mode 1)

(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat ivan-cache-directory "backups")))))
(setq backup-by-copying t)

(setq eshell-directory-name (concat ivan-cache-directory "eshell/"))

(setq custom-theme-directory (concat user-emacs-directory "themes/"))
(add-to-list 'load-path custom-theme-directory)
(load-theme 'p2 t)

(setq initial-scratch-message "")
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(tooltip-mode 0)
(set-cursor-color "#5b5b5b")

(setq whitespace-style
      (quote (face
	      empty
	      trailing
	      lines-tail
	      indentation
	      space-before-tab
	      space-after-tab)))

(global-whitespace-mode)
(setq-default indent-tabs-mode nil)
(column-number-mode nil)
(show-paren-mode 1)
(setq-default indicate-empty-lines t)

(setq-default truncate-lines t)
(defun ivan-truncate-lines-disable () (setq truncate-lines nil))
(add-hook 'help-mode-hook #'ivan-truncate-lines-disable)

(setq minibuffer-eldef-shorten-default t)
(minibuffer-electric-default-mode)
(setq read-buffer-completion-ignore-case t)

(setq require-final-newline t)

(require 'mouse)
(xterm-mouse-mode t)
(setq mouse-sel-mode t)
;;(mac-mouse-wheel-mode 0)
(defun track-mouse (e))
;; the following scroll events are overridden
;; by mac-win.el in the emacs-mac-port distro
(global-set-key [wheel-up]
		(lambda () (interactive) (scroll-down 1)))
(global-set-key [wheel-down]
		(lambda () (interactive) (scroll-up 1)))
(global-set-key [wheel-right]
		(lambda () (interactive) (scroll-left 1)))
(global-set-key [wheel-left]
		(lambda () (interactive) (scroll-right 1)))
;; OSX keys
(when (and (eq system-type 'darwin) (display-graphic-p))
       (setq mac-command-modifier 'super)
       (setq mac-option-modifier 'meta)

       ;; Keybindings
       (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
       (global-set-key (kbd "s-v") 'yank)
       (global-set-key (kbd "s-c") 'evil-yank)
       (global-set-key (kbd "s-a") 'mark-whole-buffer)
       (global-set-key (kbd "s-o") 'find-file)
       (global-set-key (kbd "s-x") 'kill-region)
       (global-set-key (kbd "s-w") 'delete-window)
       (global-set-key (kbd "s-W") 'delete-frame)
       (global-set-key (kbd "s-n") 'make-frame)
       (global-set-key (kbd "s-z") 'undo-tree-undo)
       (global-set-key (kbd "s-Z") 'undo-tree-redo)
       (global-set-key (kbd "s-s")
		       (lambda ()
			 (interactive)
			 (call-interactively (key-binding "\C-x\C-s"))))
       (global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)
       (global-set-key (kbd "M-s-h") 'mac-hide-others))

(defun mac-hide-others ()
  (interactive)
  (do-applescript (concat "tell application \"System Events\" to "
			  "set visible of every process whose visible is true "
			  "and name is not \"Emacs\" "
			  "and frontmost is false to "
			  "false")))


(set-face-attribute 'default t :font "Source Code Pro-14")
(set-frame-font "Source Code Pro-14" nil t)

(setq scroll-step 1
      scroll-conservatively 200
      mouse-wheel-scroll-amount '(0.01 ((shift) . 1)))

(scroll-bar-mode 0)

(fringe-mode '(4 . 1))

(require 'evil)
;(evil-mode t)
