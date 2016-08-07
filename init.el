(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)

(setq package-archive-priorities '(("melpa-stable" . 20) ("marmalade" . 5)))

(defun add-to-load-path (p)
  (add-to-list 'load-path (expand-file-name p user-emacs-directory)))

(defun install-use-package ()
  (package-initialize)
  (package-refresh-contents)
  (package-install 'use-package))

(defun package-dir (package)
  (let ((wildcard (concat package "*/" package ".el")))
    (car (last (file-expand-wildcards (concat user-emacs-directory "elpa/" wildcard))))))

(let ((use-package-dir (package-dir "use-package"))
      (bind-key-dir    (package-dir "bind-key")))
  (add-to-load-path (file-name-directory use-package-dir))
  (add-to-load-path (file-name-directory bind-key-dir)))

(require 'use-package)

(use-package rainbow-mode :commands rainbow-mode :load-path "elpa/rainbow-mode-0.12")

;;(defvar local-packages
;;  '(evil
;;    auto-complete
;;    use-package
;;    projectile
;;    magit
;;    hexrgb))

(defconst my-cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "Storage area for persistent files")
(unless (file-exists-p my-cache-directory)
  (make-directory my-cache-directory))

(defconst my-auto-save-directory
  (expand-file-name (concat my-cache-directory "auto-save/"))
  "Auto-save directory")
(unless (file-exists-p my-auto-save-directory)
  (make-directory my-auto-save-directory))
(setq auto-save-file-name-transforms `((".*" ,my-auto-save-directory t)))

(setq savehist-file (concat my-cache-directory "savehist"))
(savehist-mode 1)

(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat my-cache-directory "backups")))))
(setq backup-by-copying t)

(setq eshell-directory-name (concat my-cache-directory "eshell/"))

(setq custom-theme-directory (concat user-emacs-directory "themes/"))
(add-to-list 'load-path custom-theme-directory)
(load-theme 'github t)

;; transparency :: active . inactive
(set-frame-parameter (selected-frame) 'alpha '(97 . 85))
(add-to-list 'default-frame-alist '(alpha . (97 . 85)))

(setq initial-scratch-message "")
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)

(setq whitespace-line-column 90)

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
(add-hook 'help-mode-hook #'visual-line-mode)
(add-hook 'Info-mode-hook #'visual-line-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(setq minibuffer-eldef-shorten-default t)
(minibuffer-electric-default-mode)
(setq read-buffer-completion-ignore-case t)

(setq require-final-newline t)
(setq set-mark-command-repeat-pop t)

(setq tab-always-indent 'complete)
(setq ispell-program-name "aspell")

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

(defun my-buffer-face-mode-variable (height)
  "Set font to a variable width font in the current buffer"
  (interactive)
  (setq buffer-face-mode-face `(:family "Avenir Next" :height ,height))
  (buffer-face-mode))

(add-hook 'help-mode-hook (apply-partially #'my-buffer-face-mode-variable 180))
(add-hook 'Info-mode-hook (apply-partially #'my-buffer-face-mode-variable 200))

(setq frame-title-format "emacs")
(blink-cursor-mode 0)
(setq-default cursor-in-non-selected-windows nil)
(global-hl-line-mode)

(setq scroll-step 1
      scroll-margin 0
      hscroll-step 1
      hscroll-margin 2
      scroll-conservatively 200
      mouse-wheel-scroll-amount '(0.01 ((shift) . 1)))

(scroll-bar-mode 0)

(fringe-mode '(8 . 1))
(setq-default fringe-indicator-alist
              (assq-delete-all 'truncation fringe-indicator-alist))

;(require 'evil)
;(evil-mode t)

;; let Magit handle Git
(setq vc-handled-backends (delq 'Git vc-handled-backends))
