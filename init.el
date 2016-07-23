;; packages
(require 'package)

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(setq package-enable-at-startup nil)

(package-initialize)

(require 'evil)
;(evil-mode t)

(defconst ivan-cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "Storage area for persistent files")

(defconst ivan-auto-save-directory
  (expand-file-name (concat ivan-cache-directory "auto-save/"))
  "Auto-save directory")

(unless (file-exists-p ivan-cache-directory)
  (make-directory ivan-cache-directory))

(unless (file-exists-p ivan-auto-save-directory)
  (make-directory ivan-auto-save-directory))

(setq auto-save-file-name-transforms
      `((".*" ,(concat ivan-cache-directory "auto-save/") t)))

(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat ivan-cache-directory "backups")))))
(setq backup-by-copying t)

(setq savehist-file
      (concat ivan-cache-directory "savehist"))
(savehist-mode 1)


(tool-bar-mode -1)
(setq initial-scratch-message "")
(setq inhibit-startup-screen t)
(set-cursor-color "#5b5b5b")

(setq-default show-trailing-whitespace 1)
(setq-default indicate-empty-lines t)
(setq-default truncate-lines t)

(setq minibuffer-eldef-shorten-default t)
(minibuffer-electric-default-mode)

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

(set-face-attribute 'default t :font "Source Code Pro-14")
(set-frame-font "Source Code Pro-14" nil t)

(setq scroll-step 1
      scroll-conservatively 200
      mouse-wheel-scroll-amount '(0.01 ((shift) . 1)))

(fringe-mode '(4 . 1))
