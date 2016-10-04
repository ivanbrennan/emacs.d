;; blank slate
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message "ivan"
      initial-scratch-message nil
      frame-title-format "emacs")

;; If your init file is byte-compiled, use the following form instead:
;;  (eval \\='(setq inhibit-startup-echo-area-message \"YOUR-USER-NAME\"))

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
(blink-cursor-mode 0)
(setq-default cursor-type 'bar
              cursor-in-non-selected-windows nil)

;; persistence
(make-directory (concat user-emacs-directory ".cache") :mkdir_p)
(defun ivan/emacs-file (name)
  (concat user-emacs-directory name))
(make-directory (ivan/emacs-file ".cache/auto-save") :mkdir_p)
(setq custom-file (ivan/emacs-file "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

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

(setq ivan/themes '(elixir elixir-dark))
(setq ivan/themes-index 0)

(defun ivan/cycle-theme ()
  (interactive)
  (setq ivan/themes-index (% (1+ ivan/themes-index) (length ivan/themes)))
  (ivan/load-indexed-theme))

(defun ivan/load-indexed-theme ()
  (ivan/try-load-theme (nth ivan/themes-index ivan/themes)))

(defun ivan/try-load-theme (theme)
  (if (ignore-errors (load-theme theme :no-confirm))
      (mapcar #'disable-theme (remove theme custom-enabled-themes))
    (message "Unable to find theme file for ‘%s’" theme)))

(ivan/load-indexed-theme)

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
(with-current-buffer "*Messages*" (visual-line-mode))

;; splits
(setq split-width-threshold 130)

;; scroll
(setq scroll-step    1
      scroll-margin  0
      hscroll-step   1
      hscroll-margin 2
      scroll-conservatively 200
      mouse-wheel-scroll-amount '(0.01 ((shift) . 1))
      isearch-allow-scroll t)

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

(put 'mac-mwheel-scroll 'isearch-scroll t)
(put 'ivan/scroll-right 'isearch-scroll t)
(put 'ivan/scroll-left  'isearch-scroll t)
(put 'hl-line-mode      'isearch-scroll t)

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
        indicate-empty-lines     t
        show-trailing-whitespace t))

(add-hook 'prog-mode-hook #'ivan/code-whitespace)
(add-hook 'org-mode-hook  #'ivan/code-whitespace)

;; sensibility
(setq apropos-do-all                      t
      bookmark-bmenu-toggle-filenames     nil
      completion-auto-help                'lazy
      delete-by-moving-to-trash           t
      echo-keystrokes                     0.5
      ediff-split-window-function         #'split-window-horizontally
      ediff-window-setup-function         'ediff-setup-windows-plain
      find-file-visit-truename            t
      hi-lock-auto-select-face            t
      history-delete-duplicates           t
      ispell-program-name                 "/usr/local/bin/aspell"
      minibuffer-eldef-shorten-default    t
      read-buffer-completion-ignore-case  t
      require-final-newline               t
      resize-mini-windows                 t
      save-interprogram-paste-before-kill t
      scroll-preserve-screen-position     t
      sentence-end-double-space           nil
      set-mark-command-repeat-pop         t
      split-window-keep-point             nil
      tab-always-indent                   'complete
      uniquify-buffer-name-style          'forward
      vc-follow-symlinks                  t)

(fset 'yes-or-no-p 'y-or-n-p)

(defalias 'list-buffers 'ibuffer)
(minibuffer-electric-default-mode)
(delete-selection-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'tex-mode-hook #'(lambda () (setq ispell-parser 'tex)))

(defun ivan/create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory ‘%s’ does not exist! Create it?" parent-directory)))
      (make-directory parent-directory :mkdir_p))))

(add-to-list 'find-file-not-found-functions 'ivan/create-non-existent-directory)

;; env
(setenv "PAGER" "/usr/bin/env cat")

;; load-path
(add-to-list 'load-path (ivan/emacs-file "config"))

;; documentation
(with-eval-after-load 'info
  (add-to-list 'Info-additional-directory-list (ivan/emacs-file "info/")))
(with-eval-after-load 'help
  (setq source-directory "~/Development/code/elisp/emacs-mac"))

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

(setq package-enable-at-startup  nil
      package-archive-priorities '(("melpa-stable" . 2)
                                   ("gnu"          . 1)
                                   ("marmalade"    . 1)))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package rainbow-mode
  :diminish rainbow-mode
  :commands rainbow-mode
  :config (add-hook 'rainbow-mode-hook (lambda () (hl-line-mode 0))))

(use-package linum-relative
  :commands linum-relative-toggle
  :config (setq linum-relative-current-symbol ""))

(use-package elixir-mode
  :ensure t
  :config (use-package alchemist))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :commands (undo-tree-undo undo-tree-redo)
  :config
  (progn
    (setq undo-tree-history-directory-alist
          `(("." . ,(ivan/emacs-file ".cache/undo-tree-history/"))))
    (defun undo-tree-visualizer-update-linum (start end old-len)
      (if (fboundp 'linum-update)
          (linum-update undo-tree-visualizer-parent-buffer)))
    (add-hook 'undo-tree-visualizer-mode-hook
              (lambda ()
                (add-hook 'after-change-functions
                          #'undo-tree-visualizer-update-linum nil :local)))
    (undo-tree-mode 1)))

(use-package zoom-window
  :ensure t
  :commands (zoom-window-zoom
             zoom-window--enable-p)
  :config (setq zoom-window-mode-line-color "#E4FFEA"))

(defun ivan/other-window-zoom ()
  (interactive)
  (if (zoom-window--enable-p)
      (progn
        (zoom-window-zoom)
        (other-window -1))
    (other-window 1)
    (zoom-window-zoom)))

(use-package smartparens
  :commands (sp-forward-slurp-sexp
             sp-forward-barf-sexp))

(use-package evil-numbers
  :commands (evil-numbers/inc-at-pt
             evil-numbers/dec-at-pt))

(use-package evil-leader
  :ensure t
  :bind (:map evil-motion-state-map ("C-S-<SPC>" . evil-leader-mode))
  :config
  (progn
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      ","          'evil-window-next
      "9"          'rainbow-delimiters-mode
      ":"          'eval-expression
      "C-b"        'list-buffers
      "C-n"        'ivan/toggle-narrowing
      "V"          'exchange-point-and-mark
      "X <SPC>"    'server-edit
      "X s"        'server-start
      "\\"         'ivan/cycle-theme
      "a g"        'ag
      "a r"        'ag-regexp
      ;; "b <SPC>"    'hydra-buffers/body
      "b b"        'switch-to-buffer
      "b d"        'kill-this-buffer
      "b j"        'bookmark-jump
      "b l"        'bookmark-bmenu-list
      "b m"        'bookmark-set
      "f a"        'find-alternate-file
      "f j"        'dired-jump
      "f s"        'save-buffer
      "f w"        'write-file
      "g b"        'magit-blame
      "g g"        'magit-status
      "l"          'evil-switch-to-windows-last-buffer
      "m e b"      'eval-buffer
      "m e f"      'eval-defun
      "m e e"      'pp-eval-last-sexp
      "m e r"      'eval-region
      "o"          'find-file
      "s"          search-map
      "w 0"        'evil-window-delete
      "w <SPC>"    'zoom-window-zoom
      "w <return>" 'toggle-frame-fullscreen
      "w c"        'evil-window-delete
      "w j"        'webjump
      "w n"        'ivan/toggle-narrowing
      "x"          'execute-extended-command)
    (global-evil-leader-mode)))

(use-package evil-commentary
  :ensure t
  :diminish evil-commentary-mode
  :init
  (progn
    (evil-commentary-mode)
    (evil-leader/set-key ";" 'evil-commentary)))

(use-package evil
  :ensure t
  :demand
  :bind (:map evil-normal-state-map
         ("'"           . evil-goto-mark)
         ("`"           . evil-goto-mark-line)
         ("U"           . undo-tree-redo)
         ("C-r"         . isearch-backward)
         ("C-e"         . evil-end-of-line)
         ("C-S-E"       . evil-scroll-line-down)
         ("C-w C-h"     . evil-window-left)
         ("C-w C-j"     . evil-window-down)
         ("C-w C-k"     . evil-window-up)
         ("C-w C-l"     . evil-window-right)
         ("C-w s"       . split-window-below)
         ("C-w <SPC>"   . zoom-window-zoom)
         ("C-w C-<SPC>" . zoom-window-zoom)
         ("C-w S-<SPC>" . ivan/other-window-zoom)
         ("S-<SPC>"     . ivan/emacs-state-rectangle-mark-mode)
         ("C-<return>"  . ivan/add-whitespace-below)
         ("S-<return>"  . ivan/add-whitespace-above)
         ("˜"           . next-error)
         ("∏"           . previous-error)
         ("≠"           . evil-numbers/inc-at-pt)
         ("–"           . evil-numbers/dec-at-pt)
         :map evil-motion-state-map
         ("C-e"         . evil-end-of-line)
         ("C-S-E"       . evil-scroll-line-down)
         ("C-w C-h"     . evil-window-left)
         ("C-w C-j"     . evil-window-down)
         ("C-w C-k"     . evil-window-up)
         ("C-w C-l"     . evil-window-right)
         ("C-w s"       . split-window-below)
         ("C-w <SPC>"   . zoom-window-zoom)
         ("C-w C-<SPC>" . zoom-window-zoom)
         ("C-w S-<SPC>" . ivan/other-window-zoom)
         ("˜"           . next-error)
         ("∏"           . previous-error)
         :map evil-visual-state-map
         ("C-r"         . isearch-backward)
         ("<tab>"       . evil-indent)
         :map evil-insert-state-map
         ("M-v"         . yank)
         ("C-S-U"       . ivan/backward-kill-line)
         :map evil-replace-state-map
         ("M-v"         . yank)
         ("C-e"         . evil-copy-from-below)
         ("C-y"         . evil-copy-from-above))
  :config
  (progn
    (setq evil-emacs-state-tag    " ·e·"
          evil-insert-state-tag   " ·i·"
          evil-motion-state-tag   " ·m·"
          evil-normal-state-tag   " ·n·"
          evil-operator-state-tag " ·o·"
          evil-replace-state-tag  " ·r·"
          evil-visual-state-tag   " ·v·")
    (add-to-list 'evil-motion-state-modes 'ibuffer-mode)
    (setq-default evil-shift-width 2)
    (setq evil-move-cursor-back nil
          evil-emacs-state-cursor 'bar)
    (defun ivan/emacs-state-rectangle-mark-mode ()
      (interactive)
      (evil-emacs-state)
      (rectangle-mark-mode))
    (defun ivan/backward-kill-line () (kill-line 0))
    (evil-define-key 'motion help-mode-map    (kbd "<tab>") 'forward-button)
    (evil-define-key 'motion apropos-mode-map (kbd "<tab>") 'forward-button)
    (defun ivan/move-key (keymap-from keymap-to key)
      "Moves key binding from one keymap to another, deleting from the old location."
      (define-key keymap-to key (lookup-key keymap-from key))
      (define-key keymap-from key nil))
    (ivan/move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))))

(use-package goto-chg
  :commands (goto-last-change goto-last-change-reverse))

(use-package ffap
  :commands ffap-other-window)

(defun ivan/toggle-narrowing (p)
  (interactive "P")
  (cond ((and (buffer-narrowed-p) (not p))
         (widen))
        ((use-region-p)
         (narrow-to-region (region-beginning) (region-end)))
        (t
         (narrow-to-defun))))

(use-package git-link
  :config
  (progn
    (evil-leader/set-key
      "g l" 'git-link
      "g L" 'ivan/open-git-link-in-browser)
    (defun ivan/open-git-link-in-browser (remote start end)
      (interactive (let* ((remote (if current-prefix-arg
                                      (git-link--read-remote)
                                    (git-link--remote)))
                          (region (git-link--get-region)))
                     (list remote (car region) (cadr region))))
      (let ((git-link-open-in-browser t))
        (git-link remote start end)))))

(use-package hydra
  :ensure t
  :bind ("M-S-<return>" . hydra-focus/body)
  :config
  (progn
    (setq ivan/hydra-cursor-lock :always)
    (defhydra hydra-scroll (:hint nil
                            :pre (setq hydra-lv nil)
                            :post (setq-local ivan/hydra-cursor-lock :always)
                            :after-exit (setq hydra-lv t))
      "
scroll: [_SPC_]:down [_S-SPC_]:up [_j_]:down-line [_k_]:up-line [_._]:lock"
      ("SPC"   Info-scroll-up)
      ("S-SPC" Info-scroll-down)
      ("j"     ivan/scroll-next-line)
      ("k"     ivan/scroll-previous-line)
      ("."     ivan/toggle-hydra-cursor-lock)
      ("q"     nil nil)
      ("ESC"   nil nil))
    (evil-leader/set-key "." 'hydra-scroll/body)
    (defun ivan/toggle-hydra-cursor-lock ()
      (interactive)
      (setq-local ivan/hydra-cursor-lock
                  (if (eql :always ivan/hydra-cursor-lock) t :always)))
    (defun ivan/scroll-previous-line ()
      (interactive)
      (let ((scroll-preserve-screen-position ivan/hydra-cursor-lock))
        (scroll-down-line)))
    (defun ivan/scroll-next-line ()
      (interactive)
      (let ((scroll-preserve-screen-position ivan/hydra-cursor-lock))
        (scroll-up-line)))
    (defhydra hydra-focus ()
      "focus"
      ("]"   ivan/increase-padding "increase")
      ("["   ivan/reduce-padding   "reduce")
      ("RET" ivan/toggle-padding   "toggle")
      ("q"   nil "quit" :color blue)
      ("ESC" nil "quit" :color blue))))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (progn
    (which-key-declare-prefixes
      "<SPC> b"   "buffers/bookmarks"
      "<SPC> f"   "files"
      "<SPC> g"   "git"
      "<SPC> m"   "mode"
      "<SPC> m e" "eval"
      "<SPC> s"   "search"
      "<SPC> ."   "scroll")))

(use-package drag-stuff
  :demand
  :diminish drag-stuff-mode
  :config (drag-stuff-global-mode t))

(use-package company
  :bind (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)))

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
  (progn
    (use-package dash)
    (use-package s)))

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
  (progn
    (setq windsize-rows 1 windsize-cols 2)
    (put 'windsize-down  'isearch-scroll t)
    (put 'windsize-up    'isearch-scroll t)
    (put 'windsize-left  'isearch-scroll t)
    (put 'windsize-right 'isearch-scroll t)))

(defun ivan/setup-org-mode ()
  (setq org-hide-leading-stars t)
  (variable-pitch-mode t)
  (setq line-spacing 0.15))
(add-hook 'org-mode-hook #'ivan/setup-org-mode)

(use-package org-bullets
  :ensure t
  :config
  (progn
    (setq org-bullets-bullet-list '("◉" "○" "•"))
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :commands page-break-lines-mode
  :init
  (progn
    (add-hook 'help-mode-hook #'page-break-lines-mode)
    (add-hook 'Info-mode-hook #'page-break-lines-mode)))

(use-package evil-magit
  :demand
  :init
  (progn
    (setq evil-magit-use-y-for-yank nil
          evil-magit-want-horizontal-movement t))
  :config
  (progn
    (evil-define-key evil-magit-state magit-mode-map
      (kbd "n")   'magit-section-forward
      (kbd "p")   'magit-section-backward
      (kbd "P")   'magit-push-popup
      (kbd "C-w") 'evil-window-map
      (kbd "y")   nil
      (kbd "yy")  'evil-yank-line
      (kbd "yr")  'magit-show-refs-popup
      (kbd "ys")  'magit-copy-section-value
      (kbd "yb")  'magit-copy-buffer-revision)))

(use-package magit
  :ensure t
  :config
  (progn
    (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
          magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))))

;; gui & terminal
(defun ivan/text-scale-reset ()
  "Reset the height of the default face in the current buffer to its default value.
Disables `text-scale-mode`."
  (interactive)
  (text-scale-set 0))

(defun configure-gui ()
  (bind-keys ("M-q"   . save-buffers-kill-terminal)
             ("M-A"   . mark-whole-buffer)
             ("M-o"   . find-file)
             ("M-c"   . kill-ring-save)
             ("M-w"   . delete-window)
             ("M-W"   . delete-frame)
             ("M-N"   . make-frame)
             ("M-s"   . save-buffer)
             ("M-u"   . ivan/toggle-transparency)
             ("M-="   . text-scale-increase)
             ("M--"   . text-scale-decrease)
             ("M-0"   . ivan/text-scale-reset)
             ("M-…"   . hl-line-mode) ; (⌥⌘;)
             ("M-`"   . ns-next-frame)
             ("M-~"   . ns-prev-frame)
             ("M-<return>" . toggle-frame-fullscreen)
             ;; reconcile some overridden keybindings
             ("ESC M-q" . fill-paragraph)
             ("ESC M-o" . facemenu-keymap)
             ("ESC M-c" . capitalize-word)
             ("ESC M-u" . upcase-word)
             ("ESC M-=" . count-words-region))
  (bind-key "ESC M-s" search-map))

(defun configure-terminal ()
  (xterm-mouse-mode)
  (setq mouse-sel-mode t
        mouse-wheel-follow-mouse 't)

  (bind-keys ("<mouse-4>" . scroll-down-line)
             ("<mouse-5>" . scroll-up-line))

  (put 'scroll-down-line 'isearch-scroll t)
  (put 'scroll-up-line   'isearch-scroll t)

  (defun ivan/adjust-terminal-colors ()
    (unless (display-graphic-p (selected-frame))
      (set-face-background 'default "white" (selected-frame))
      (set-face-background 'hl-line "#EEEEEE" (selected-frame))))

  (add-hook 'window-setup-hook 'ivan/adjust-terminal-colors))

(defun system-is-mac () (eq system-type 'darwin))

(defun configure-mac-modifiers ()
  (setq mac-command-modifier 'meta))

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
(bind-keys ("M-/" . hippie-expand)
           ("C-/" . undo-tree-undo)
           ("C-?" . undo-tree-redo)
           ("C-w" . ivan/kill-region-or-backward-kill-word)
           ("S-<SPC>" . rectangle-mark-mode)
           :map rectangle-mark-mode-map
           ("s" . string-rectangle)
           ("o" . rectangle-exchange-point-and-mark)
           :map shell-mode-map
           ("C-d" . comint-delchar-or-eof-or-kill-buffer))

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(bind-key "<escape>" 'isearch-abort isearch-mode-map)

(mapc (lambda (keymap)
        (define-key keymap [escape]    'minibuffer-keyboard-quit)
        (define-key keymap (kbd "C-p") 'previous-line-or-history-element)
        (define-key keymap (kbd "C-n") 'next-line-or-history-element))
      (list
       minibuffer-local-map
       minibuffer-local-ns-map
       minibuffer-local-completion-map
       minibuffer-local-must-match-map
       minibuffer-local-isearch-map))

(defun ivan/isearch-exit ()
  "Run isearch-exit, and if in the minibuffer, submit the search result as input."
  (interactive)
  (isearch-exit)
  (if (minibuffer-window-active-p (selected-window))
      (minibuffer-complete-and-exit)))

;; padding
(set-display-table-slot
 standard-display-table 0 ?\ )
(setq-default
 fringe-indicator-alist (assq-delete-all 'truncation fringe-indicator-alist))

(setq ivan/padding-enabled nil)
(setq ivan/padding-min 4)
(setq ivan/padding-max 580)
(setq ivan/padding-step 32)
(setq ivan/padding-degree ivan/padding-min)

(add-to-list 'default-frame-alist `(left-fringe . ,ivan/padding-min))
(add-to-list 'default-frame-alist '(right-fringe . 1))

(defun ivan/increase-padding ()
  (interactive)
  (setq-local ivan/padding-degree
              (min ivan/padding-max (+ ivan/padding-degree ivan/padding-step)))
  (ivan//apply-padding-degree ivan/padding-degree)
  (setq-local ivan/padding-enabled t))

(defun ivan/reduce-padding ()
  (interactive)
  (setq-local ivan/padding-degree
              (max ivan/padding-min (- ivan/padding-degree ivan/padding-step)))
  (ivan//apply-padding-degree ivan/padding-degree)
  (setq-local ivan/padding-enabled t))

(defun ivan/toggle-padding ()
  (interactive)
  (ivan//apply-padding-degree
   (if ivan/padding-enabled ivan/padding-min ivan/padding-degree))
  (setq-local ivan/padding-enabled (not ivan/padding-enabled)))

(defun ivan//apply-padding-degree (n) (set-window-fringes nil n))

(with-eval-after-load "isearch"
  (define-key isearch-mode-map [remap isearch-exit] #'ivan/isearch-exit))

(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(defun ivan/add-whitespace-below ()
  (interactive)
  (save-excursion
    (end-of-line)
    (newline)))

(defun ivan/add-whitespace-above ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (newline)
    (forward-line -1)))

(defun ivan/minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun ivan/minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'ivan/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'ivan/minibuffer-exit-hook)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(with-eval-after-load 'webjump
  (setq webjump-sites
        (append '(("stackoverflow" . "www.stackoverflow.com")
                  ("GitHub" . "https://github.com"))
                webjump-sample-sites)))

;; etc.
(add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

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
(with-eval-after-load 'flyspell (diminish 'flyspell-mode))
(with-eval-after-load 'autorevert (diminish 'auto-revert-mode))

(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(with-eval-after-load 'ibuffer (require 'ibuffer-config))
(with-eval-after-load 'dired (require 'dired-config))

(add-hook 'after-init-hook #'evil-mode)
