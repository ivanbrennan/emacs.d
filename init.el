;; blank slate
(setq initial-scratch-message "")
(setq inhibit-startup-screen t)
(setq frame-title-format "emacs")


;; clean screen
(menu-bar-mode   0)
(tool-bar-mode   0)
(scroll-bar-mode 0)
(tooltip-mode    0)


;; useful indicators
(column-number-mode nil)
(show-paren-mode    1)


;; cursor
(blink-cursor-mode 0)
(setq-default cursor-in-non-selected-windows nil)
(global-hl-line-mode)


;; fringe
(fringe-mode '(8 . 1))
(setq-default fringe-indicator-alist
              (assq-delete-all 'truncation fringe-indicator-alist))


;; faces
(setq custom-theme-directory (concat user-emacs-directory "themes/"))
(add-to-list 'load-path custom-theme-directory)
(load-theme 'github t)

(set-face-attribute 'default t :font "Source Code Pro-16")
(set-frame-font "Source Code Pro-16" nil t)

(defun ivan/buffer-face-mode-variable (height)
  "Set font to a variable width font in the current buffer"
  (interactive)
  (setq buffer-face-mode-face `(:family "Avenir Next" :height ,height))
  (buffer-face-mode))

(add-hook 'help-mode-hook (apply-partially #'ivan/buffer-face-mode-variable 180))
(add-hook 'Info-mode-hook (apply-partially #'ivan/buffer-face-mode-variable 200))


;; transparency
(set-frame-parameter (selected-frame) 'alpha '(97 . 85))
(add-to-list 'default-frame-alist   '(alpha . (97 . 85)))


;; line-wrapping
(setq-default truncate-lines t)
(add-hook 'help-mode-hook #'visual-line-mode)
(add-hook 'Info-mode-hook #'visual-line-mode)


;; scroll
(setq scroll-step 1
      scroll-margin 0
      hscroll-step 1
      hscroll-margin 2
      scroll-conservatively 200
      mouse-wheel-scroll-amount '(0.01 ((shift) . 1)))


;; whitespace
(setq whitespace-style
      (quote (face
              empty
              trailing
              lines-tail
              indentation
              space-before-tab
              space-after-tab)))

(global-whitespace-mode)
(setq whitespace-line-column 90)
(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)


;; persistence
(defconst ivan/cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "Storage area for persistent files")
(unless (file-exists-p ivan/cache-directory)
  (make-directory ivan/cache-directory))

(defconst ivan/auto-save-directory
  (expand-file-name (concat ivan/cache-directory "auto-save/"))
  "Auto-save directory")
(unless (file-exists-p ivan/auto-save-directory)
  (make-directory ivan/auto-save-directory))
(setq auto-save-file-name-transforms
      `((".*" ,ivan/auto-save-directory t)))

(setq savehist-file (concat ivan/cache-directory "savehist"))
(savehist-mode 1)

(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat ivan/cache-directory "backups")))))
(setq backup-by-copying t)

(setq eshell-directory-name (concat ivan/cache-directory "eshell/"))


;; sensibility
(setq minibuffer-eldef-shorten-default t)
(minibuffer-electric-default-mode)
(setq read-buffer-completion-ignore-case t)
(setq require-final-newline t)
(setq set-mark-command-repeat-pop t)
(setq tab-always-indent 'complete)
(setq ispell-program-name "aspell")
(setq ediff-split-window-function 'split-window-horizontally)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)


;; gui & terminal
(defun system-is-mac () (eq system-type 'darwin))

(defun configure-mac-modifiers ()
       (setq mac-command-modifier 'super)
       (setq mac-option-modifier 'meta))

;; turn off "displays have separate spaces" in osx
;; so fullscreen won't black out other monitors.
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

;; this is slow :P
(defun mac-hide-others ()
  (interactive)
  (do-applescript (concat "tell application \"System Events\" to "
                          "set visible of every process whose visible is true "
                          "and name is not \"Emacs\" "
                          "and frontmost is false to "
                          "false")))

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

(if (system-is-mac)
    (configure-mac-modifiers))

(if (display-graphic-p)
    (configure-gui)
  (configure-terminal))


;; packages
(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)

(setq package-archive-priorities '(("melpa-stable" . 20) ("marmalade" . 5)))
(setq package-enable-at-startup nil)

(defun package-path (package)
  "Return the path of the highest installed version of PACKAGE,
or nil if no installed versions are found."
  (let* ((name (symbol-name package))
         (path (concat user-emacs-directory
                       "elpa/"
                       (concat name "*/" name ".el"))))
         (car (last (file-expand-wildcards path)))))

(require 'cl)
(defun missing-packages (package-list)
  (remove-if #'package-path package-list))

(defun install-packages (packages)
  (package-initialize)
  (package-refresh-contents)
  (mapc 'package-install packages))

(let* ((essentials '(use-package evil))
       (missing-essentials (missing-packages essentials)))
  (if missing-essentials (install-packages missing-essentials)))

(defun add-package-to-load-path (p)
  (let ((dir (file-name-directory (package-path p))))
    (add-to-list 'load-path
                 (expand-file-name dir user-emacs-directory))))

(add-package-to-load-path 'use-package)
(add-package-to-load-path 'bind-key)

(require 'use-package)

(use-package rainbow-mode
  :load-path "elpa/rainbow-mode-0.12"
  :commands rainbow-mode)

(use-package evil
  :load-path "elpa/evil-1.2.12"
  :commands evil-mode)

;;(use-package magit...
;;(use-package auto-complete...
;;(use-package projectile...


;; etc.
(defun ivan/goto-match-beginning ()
  (when (and isearch-forward isearch-other-end (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))
(add-hook 'isearch-mode-end-hook 'ivan/goto-match-beginning)

;; let Magit handle Git
(setq vc-handled-backends (delq 'Git vc-handled-backends))
