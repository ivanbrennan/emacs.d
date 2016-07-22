;; packages
(require 'package)

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(setq package-enable-at-startup nil)

(package-initialize)

;; vim
(require 'evil)
(evil-mode t)

;; .cache
(defconst my-cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "Storage area for persistent files")

(defconst my-auto-save-directory
  (expand-file-name (concat my-cache-directory "auto-save/"))
  "Auto-save directory")

(unless (file-exists-p my-cache-directory)
  (make-directory my-cache-directory))

(unless (file-exists-p my-auto-save-directory)
  (make-directory my-auto-save-directory))

(setq auto-save-file-name-transforms
      `((".*" ,(concat my-cache-directory "auto-save/") t)))

(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat my-cache-directory "backups")))))
(setq backup-by-copying t)

(setq savehist-file
      (concat my-cache-directory "savehist"))
(savehist-mode 1)

;; scratch
(setq initial-scratch-message "")

;; hide the toolbar
(tool-bar-mode -1)

;; minibuffer
(setq minibuffer-eldef-shorten-default t)
(minibuffer-electric-default-mode)

;; no startup screen
(setq inhibit-startup-screen t)

;; cursor
(set-cursor-color "#5b5b5b")

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
