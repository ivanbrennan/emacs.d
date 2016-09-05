;; blank slate
(setq initial-scratch-message nil
      inhibit-startup-screen t
      frame-title-format "emacs")
(setq inhibit-startup-echo-area-message "ivan")

;; clean screen
(menu-bar-mode   0)
(tool-bar-mode   0)
(scroll-bar-mode 0)
(tooltip-mode    0)

;; useful indicators
(column-number-mode)

;; parens
(show-paren-mode)
(electric-pair-mode)
(setq blink-matching-paren 'jump
      blink-matching-delay 0.25)

;; cursor
(setq-default cursor-type 'bar
              cursor-in-non-selected-windows nil)
(blink-cursor-mode 0)

;; persistence
(make-directory (concat user-emacs-directory ".cache") :mkdir_p)
(defun ivan/emacs-file (name) (concat user-emacs-directory name))
(make-directory (ivan/emacs-file ".cache/auto-save") :mkdir_p)

(setq backup-directory-alist         `(("." . ,(ivan/emacs-file ".cache/backups/")))
      auto-save-file-name-transforms `((".*" ,(ivan/emacs-file ".cache/auto-save/") :uniquify))
      auto-save-list-file-prefix      (ivan/emacs-file ".cache/auto-save-list/.saves-")
      savehist-file                   (ivan/emacs-file ".cache/savehist")
      ido-save-directory-list-file    (ivan/emacs-file ".cache/ido.last")
      eshell-directory-name           (ivan/emacs-file ".cache/eshell/")
      tramp-persistency-file-name     (ivan/emacs-file ".cache/tramp")
      backup-by-copying t)

(savehist-mode)

;; theme
(setq custom-theme-directory (ivan/emacs-file "themes/"))
(make-directory custom-theme-directory :mkdir_p)

(let ((theme 'github))
  (unless (ignore-errors (load-theme theme :no-confirm))
  (message "Unable to find theme file for ‘%s’" theme)))

(add-hook 'help-mode-hook #'variable-pitch-mode)
(add-hook 'Info-mode-hook #'variable-pitch-mode)

;; transparency
(let ((transparent '(97 . 85)))
  (set-frame-parameter (selected-frame) 'alpha transparent)
  (add-to-list 'default-frame-alist `(alpha . ,transparent)))

(defun ivan/toggle-transparency ()
  (interactive)
  (let ((new-value (if (eql (frame-parameter nil 'alpha) 100) '(97 . 85) 100)))
    (set-frame-parameter nil 'alpha new-value)))

;; line-wrapping
(defun ivan/truncate-lines () (setq truncate-lines t))
(add-hook 'prog-mode-hook #'ivan/truncate-lines)
(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'help-mode-hook #'visual-line-mode)
(add-hook 'Info-mode-hook #'visual-line-mode)
(with-current-buffer "*Messages*" (visual-line-mode))

;; splits
(setq split-width-threshold 130)

;; scroll
(setq scroll-step 1
      scroll-margin 0
      hscroll-step 1
      hscroll-margin 2
      scroll-conservatively 200
      mouse-wheel-scroll-amount '(0.01 ((shift) . 1)))

(defun ivan/scroll-right ()
  (interactive)
  (scroll-right 2))
(defun ivan/scroll-left ()
  (interactive)
  (scroll-left 2))

(defvar ivan/hscroll-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [wheel-right] #'ivan/scroll-left)
    (define-key map [wheel-left] #'ivan/scroll-right)
    map)
  "ivan/hscroll-minor-mode keymap.")
(define-minor-mode ivan/hscroll-minor-mode
  "A minor mode so my horizontal scroll bindings take precedence."
  :init-value t)
(ivan/hscroll-minor-mode 1)

;; whitespace
(setq whitespace-line-column 90
      whitespace-style '(face
                         empty
                         trailing
                         lines-tail
                         indentation
                         space-before-tab
                         space-after-tab))

(defun ivan/code-whitespace ()
  (hl-line-mode)
  (setq indent-tabs-mode         nil
        show-trailing-whitespace t
        indicate-empty-lines     t))
(add-hook 'prog-mode-hook #'ivan/code-whitespace)
(add-hook 'org-mode-hook  #'ivan/code-whitespace)

;; sensibility
(setq read-buffer-completion-ignore-case  t
      require-final-newline               t
      set-mark-command-repeat-pop         t
      tab-always-indent                   'complete
      ispell-program-name                 "/usr/local/bin/aspell"
      ediff-split-window-function         #'split-window-horizontally
      minibuffer-eldef-shorten-default    t
      uniquify-buffer-name-style          'forward
      save-interprogram-paste-before-kill t
      apropos-do-all                      t
      delete-by-moving-to-trash           t
      ediff-window-setup-function         'ediff-setup-windows-plain
      sentence-end-double-space           nil)

(fset 'yes-or-no-p 'y-or-n-p)

(defalias 'list-buffers 'ibuffer)
(minibuffer-electric-default-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'tex-mode-hook #'(lambda () (setq ispell-parser 'tex)))

;; env
(setenv "PAGER" "/usr/bin/env cat")

;; documentation
(eval-after-load 'info
  '(add-to-list 'Info-additional-directory-list (ivan/emacs-file "info/")))

;; tramp
(setq tramp-default-method "ssh")

;; packages
(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/")
             :append)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/")
             :append)

(setq package-archive-priorities '(("melpa-stable" . 2) ("gnu" . 1) ("marmalade" . 1))
      package-enable-at-startup  nil)

(package-initialize)
(setq package-enable-at-startup nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package rainbow-mode
  :diminish rainbow-mode
  :commands rainbow-mode)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :commands (undo-tree-undo undo-tree-redo)
  :config
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist `(("." . ,(ivan/emacs-file ".cache/undo-tree-history/"))))
  (defun undo-tree-visualizer-update-linum (start end old-len)
    (if (fboundp 'linum-update)
        (linum-update undo-tree-visualizer-parent-buffer)))
  (add-hook 'undo-tree-visualizer-mode-hook
            (lambda () (add-hook 'after-change-functions #'undo-tree-visualizer-update-linum nil :local)))
  (undo-tree-mode 1))

(use-package zoom-window
  :ensure t
  :commands zoom-window-zoom
  :config
  (setq zoom-window-mode-line-color "#E4FFEA"))

(use-package evil
  :ensure t
  :demand
  :bind (:map evil-normal-state-map
         ("U"       . undo-tree-redo)
         ("C-r"     . isearch-backward-regexp)
         ("C-w C-h" . evil-window-left)
         ("C-w C-j" . evil-window-down)
         ("C-w C-k" . evil-window-up)
         ("C-w C-l" . evil-window-right)
         ("C-w O"   . delete-other-windows)
         ("C-w o"   . zoom-window-zoom)
         ("C-w C-o" . zoom-window-zoom)
         ("C-w C-w" . zoom-window-zoom)
         :map evil-motion-state-map
         ("C-w C-h" . evil-window-left)
         ("C-w C-j" . evil-window-down)
         ("C-w C-k" . evil-window-up)
         ("C-w C-l" . evil-window-right)
         ("C-w O"   . delete-other-windows)
         ("C-w o"   . zoom-window-zoom)
         ("C-w C-o" . zoom-window-zoom)
         ("C-w C-w" . zoom-window-zoom)
         :map evil-visual-state-map
         ("C-r"     . isearch-backward-regexp)
         ("<tab>"   . evil-indent))
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (setq-default evil-shift-width 2)
  (setq evil-move-cursor-back nil
        evil-emacs-state-cursor 'bar)
  (defun ivan/move-key (keymap-from keymap-to key)
    "Moves key binding from one keymap to another, deleting from the old location."
    (define-key keymap-to key (lookup-key keymap-from key))
    (define-key keymap-from key nil))
  (ivan/move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
  (use-package goto-chg
    :commands (goto-last-change goto-last-change-reverse))
  (use-package ffap
    :commands ffap-other-window)
  (use-package evil-leader
    :ensure t
    :config
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key "x"  'execute-extended-command)
    (evil-leader/set-key "fs" 'save-buffer)
    (global-evil-leader-mode))
  (evil-mode))

(use-package ag
  :ensure t
  :commands (ag
             ag-files
             ag-regexp
             ag-project
             ag-project-files
             ag-project-regexp
             ag-dired
             ag-dired-regexp
             ag-project-dired
             ag-project-dired-regexp)
  :config
  (use-package dash)
  (use-package s))

(use-package crux
  :ensure t
  :commands (crux-smart-open-line
             crux-smart-open-line-above)
  :bind (("C-<return>" . crux-smart-open-line)
         ("S-<return>" . crux-smart-open-line-above)))

(use-package windsize
  :ensure t
  :bind (("C-S-<left>"  . windsize-left)
         ("C-S-<right>" . windsize-right)
         ("C-S-<up>"    . windsize-up)
         ("C-S-<down>"  . windsize-down))
  :config
  (setq windsize-rows 1 windsize-cols 2))

(defun ivan/setup-org-mode ()
  (setq org-hide-leading-stars t)
  (variable-pitch-mode t)
  (setq line-spacing 0.15))
(add-hook 'org-mode-hook #'ivan/setup-org-mode)
(use-package org-bullets
  :ensure t
  :config
  (setq org-bullets-bullet-list '("◉" "○" "•"))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :commands page-break-lines-mode
  :init
  (add-hook 'help-mode-hook #'page-break-lines-mode)
  (add-hook 'Info-mode-hook #'page-break-lines-mode))

(use-package magit
  :ensure t)

;; gui & terminal
(defun ivan/text-scale-reset ()
  "Reset the height of the default face in the current buffer to its default value.
Disables `text-scale-mode`."
  (interactive)
  (text-scale-set 0))

(defun ivan/local-toggle-hl-line ()
  "Toggle line highlighting in current buffer"
  (interactive)
  (setq hl-line-mode (null hl-line-mode)))

(defun configure-gui ()
  (bind-keys ("M-q"   . save-buffers-kill-terminal)
             ("M-s-a" . mark-whole-buffer)
             ("M-o"   . find-file)
             ("M-w"   . delete-window)
             ("M-W"   . delete-frame)
             ("M-n"   . make-frame)
             ("M-s"   . save-buffer)
             ("M-u"   . ivan/toggle-transparency)
             ("M-="   . text-scale-increase)
             ("M--"   . text-scale-decrease)
             ("M-0"   . ivan/text-scale-reset)
             ("M-s-;" . ivan/local-toggle-hl-line)
             ("M-s-h" . mac-hide-others)
             ;; turn off "displays have separate spaces" so
             ;; fullscreen won't black out other monitors.
             ("M-<return>" . toggle-frame-fullscreen)
             ;; reconcile some overridden keybindings
             ("s-q"   . fill-paragraph)
             ("s-o"   . facemenu-keymap)
             ("s-w"   . kill-ring-save)
             ("s-s"   . search-map)
             ("s-u"   . upcase-word)
             ("s-="   . count-words-region)))

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
  (xterm-mouse-mode)
  (defun track-mouse (e))
  (setq mouse-sel-mode t
        mouse-wheel-follow-mouse 't)
  (defvar alternating-scroll-down-next t)
  (defvar alternating-scroll-up-next t)
  (bind-keys ("<mouse-4>" . alternating-scroll-down-line)
             ("<mouse-5>" . alternating-scroll-up-line))

  (defun alternating-scroll-down-line ()
    (interactive "@")
    (when alternating-scroll-down-next
      (scroll-down-line))
    (setq alternating-scroll-down-next
          (not alternating-scroll-down-next)))

  (defun alternating-scroll-up-line ()
    (interactive "@")
    (when alternating-scroll-up-next
      (scroll-up-line))
    (setq alternating-scroll-up-next
          (not alternating-scroll-up-next))))

(defun system-is-mac () (eq system-type 'darwin))

(defun configure-mac-modifiers ()
  (setq mac-command-modifier 'meta
        mac-option-modifier  'super))

(defun configure-mac-directory-program ()
  (if (file-exists-p "/usr/local/bin/gls")
      (setq insert-directory-program "/usr/local/bin/gls")
    (require 'ls-lisp)
    (setq ls-lisp-use-insert-directory-program nil)))

(if (system-is-mac)
    (progn
      (configure-mac-modifiers)
      (configure-mac-directory-program)))

(if (display-graphic-p)
    (configure-gui)
  (configure-terminal))

;; keybindings
(bind-keys ("C-M-s" . isearch-forward)
           ("C-M-r" . isearch-backward)
           ("C-s" . isearch-forward-regexp)
           ("C-r" . isearch-backward-regexp)
           ("M-/" . hippie-expand)
           ("C-/" . undo-tree-undo)
           ("C-?" . undo-tree-redo)
           ("C-w" . ivan/kill-region-or-backward-kill-word))

(defun ivan/isearch-exit ()
  "Run isearch-exit, and if in the minibuffer, submit the search result as input."
  (interactive)
  (isearch-exit)
  (if (minibuffer-window-active-p (selected-window))
      (minibuffer-complete-and-exit)))
(defun ivan/remap-isearch-exit ()
  (define-key
    overriding-terminal-local-map
    [remap isearch-exit]
    #'ivan/isearch-exit))
(add-hook 'isearch-mode-hook #'ivan/remap-isearch-exit)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; etc.
(defun ivan/goto-match-beginning ()
  (when (and isearch-forward isearch-other-end
             (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))
(add-hook 'isearch-mode-end-hook #'ivan/goto-match-beginning)

;; more useful C-w (this should be adjusted to account for evil mode,
;; in particular insert-state, once I start using evil).
(defun ivan/kill-region-or-backward-kill-word (&optional arg region)
  "`kill-region' if the region is active, otherwise `backward-kill-word'"
  (interactive
   (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

;; diminish
(eval-after-load "flyspell" '(diminish 'flyspell-mode))
(eval-after-load "autorevert" '(diminish 'auto-revert-mode))

(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
