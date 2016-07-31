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
(load-theme 'sanityinc-tomorrow-bright t)

;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
(set-frame-parameter (selected-frame) 'alpha '(98 . 88))
(add-to-list 'default-frame-alist '(alpha . (98 . 88)))

(setq initial-scratch-message "")
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(tooltip-mode 0)
;(set-cursor-color "#eeeeee")

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
(setq set-mark-command-repeat-pop t)

(defun system-is-mac () (eq system-type 'darwin))

;; go to System Preferences > Mission Control
;; and uncheck "Displays have separate spaces"
;; so fullscreen won't black out other monitors
(defun configure-mac-modifiers ()
       (setq mac-command-modifier 'super)
       (setq mac-option-modifier 'meta))

(defun configure-gui ()
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
  (global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)
  (global-set-key (kbd "M-s-h") 'mac-hide-others))

(defun configure-terminal ()
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  (setq mouse-wheel-follow-mouse 't)
  (defvar alternating-scroll-down-next t)
  (defvar alternating-scroll-up-next t)
  (global-set-key (kbd "<mouse-4>") 'alternating-scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'alternating-scroll-up-line)

  (defun alternating-scroll-down-line ()
    (interactive "@")
    (when alternating-scroll-down-next
      (scroll-down-line))
    (setq alternating-scroll-down-next (not alternating-scroll-down-next)))

  (defun alternating-scroll-up-line ()
    (interactive "@")
    (when alternating-scroll-up-next
      (scroll-up-line))
    (setq alternating-scroll-up-next (not alternating-scroll-up-next))))

(defun mac-hide-others ()
  (interactive)
  (do-applescript (concat "tell application \"System Events\" to "
			  "set visible of every process whose visible is true "
			  "and name is not \"Emacs\" "
			  "and frontmost is false to "
			  "false")))

(if (system-is-mac)
    (configure-mac-modifiers))

(if (display-graphic-p)
    (configure-gui)
  (configure-terminal))

(set-face-attribute 'default t :font "Source Code Pro-16")
(set-frame-font "Source Code Pro-16" nil t)

(setq scroll-step 1
      scroll-conservatively 200
      mouse-wheel-scroll-amount '(0.01 ((shift) . 1)))

(scroll-bar-mode 0)

(fringe-mode '(4 . 1))

(require 'evil)
;(evil-mode t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#151719" :foreground "#eaeaea" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 161 :width normal :foundry "nil" :family "Source Code Pro")))))
