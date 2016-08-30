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

(defun package-path (pkg)
  "Return the path of the highest installed version of package PKG,
or nil if no installed versions are found."
  (let ((name (symbol-name pkg)))
    (car (last
          (file-expand-wildcards
           (concat package-user-dir "/" name "*/" name ".el"))))))

(defun add-package-to-load-path (pkg)
  (add-to-list 'load-path
               (directory-file-name
                (file-name-directory (package-path pkg)))))

(unless (package-path 'use-package)
  (package-initialize)
  (package-refresh-contents)
  (package-install 'use-package))

(add-package-to-load-path 'use-package)
(add-package-to-load-path 'bind-key)

(require 'use-package)

(use-package rainbow-mode
  :load-path "elpa/rainbow-mode-0.12"
  :commands rainbow-mode)

(use-package undo-tree
  :load-path "elpa/undo-tree-0.6.5"
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

(use-package evil
  :load-path "elpa/evil-1.2.12"
  :demand
  :bind (:map evil-normal-state-map
         ("U"       . undo-tree-redo)
         ("C-w C-h" . evil-window-left)
         ("C-w C-j" . evil-window-down)
         ("C-w C-k" . evil-window-up)
         ("C-w C-l" . evil-window-right)
         :map evil-motion-state-map
         ("C-w C-h" . evil-window-left)
         ("C-w C-j" . evil-window-down)
         ("C-w C-k" . evil-window-up)
         ("C-w C-l" . evil-window-right)
         :map evil-visual-state-map
         ("<tab>"   . evil-indent))
  :init
  (setq ad-redefinition-action 'accept) ; silence evil-mode redefinition warnings
  (setq-default evil-shift-width 2)
  (setq evil-move-cursor-back nil
        evil-want-C-u-scroll t
        evil-emacs-state-cursor 'bar)
  (use-package goto-chg
    :load-path "elpa/goto-chg-1.6"
    :commands (goto-last-change goto-last-change-reverse))
  (use-package ffap
    :commands ffap-other-window)
  (use-package evil-leader
    :load-path "elpa/evil-leader-0.4.3"
    :config
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key "x" 'execute-extended-command)
    (global-evil-leader-mode))
  :config
  (setq ad-redefinition-action 'warn) ; return to the default behavior
  (defun ivan/move-key (keymap-from keymap-to key)
    "Moves key binding from one keymap to another, deleting from the old location."
    (define-key keymap-to key (lookup-key keymap-from key))
    (define-key keymap-from key nil))
  (ivan/move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
  (evil-mode))

(use-package ag
  :load-path "elpa/ag-0.47"
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
  :init
  (use-package dash :load-path "elpa/dash-2.13.0")
  (use-package s    :load-path "elpa/s-1.11.0"))

(use-package windsize
  :load-path "elpa/windsize-0.1"
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
  :load-path "elpa/org-bullets-0.2.4"
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package page-break-lines
  :load-path "elpa/page-break-lines-0.11"
  :commands page-break-lines-mode
  :config
  (add-hook 'help-mode-hook #'page-break-lines-mode)
  (add-hook 'Info-mode-hook #'page-break-lines-mode))

(use-package magit
  :load-path ("elpa/magit-2.8.0"
              "elpa/with-editor-2.5.2"
              "elpa/git-commit-2.8.0"
              "elpa/magit-popup-2.8.0")
  :config
  (setq vc-handled-backends (delq 'Git vc-handled-backends))
  (eval-after-load 'info
    '(add-to-list 'Info-additional-directory-list (ivan/emacs-file "elpa/magit-2.8.0/"))))

;; gui & terminal
(defun ivan/text-scale-reset ()
  "Reset the height of the default face in the current buffer to its default value.
Disables `text-scale-mode`."
  (interactive)
  (text-scale-set 0))

(defun ivan/local-toggle-hl-line ()
  "Toggle line highlighting in current buffer"
  (interactive)
  (setq-local global-hl-line-mode
              (null global-hl-line-mode)))

(defun configure-gui ()
  (bind-keys ("s-q" . save-buffers-kill-terminal)
             ("s-v" . yank)
             ("s-c" . evil-yank)
             ("s-a" . mark-whole-buffer)
             ("s-o" . find-file)
             ("s-x" . kill-region)
             ("s-w" . delete-window)
             ("s-W" . delete-frame)
             ("s-n" . make-frame)
             ("s-s" . save-buffer)
             ("s-u" . ivan/toggle-transparency)
             ("s-=" . text-scale-increase)
             ("s--" . text-scale-decrease)
             ("s-0" . ivan/text-scale-reset)
             ("M-s-;" . ivan/local-toggle-hl-line)
             ("M-s-h" . mac-hide-others)
             ;; turn off "displays have separate spaces" so
             ;; fullscreen won't black out other monitors.
             ("s-<return>" . toggle-frame-fullscreen))
  (bind-keys :map minibuffer-local-map
             ("s-p" . previous-history-element)
             ("s-n" . next-history-element)))

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
  (setq mac-command-modifier 'super
        mac-option-modifier  'meta))

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
(bind-keys ("C-<return>" . crux-mini-smart-open-line)
           ("S-<return>" . crux-mini-smart-open-line-above)
           ("C-M-s" . isearch-forward)
           ("C-M-r" . isearch-backward)
           ("C-s" . isearch-forward-regexp)
           ("C-r" . isearch-backward-regexp)
           ("M-/" . hippie-expand)
           ("C-/" . undo-tree-undo)
           ("C-?" . undo-tree-redo)
           ("C-w" . ivan/kill-region-or-backward-kill-word))

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

;;; crux-mini (lifted from crux)
(defun crux-mini-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (insert "\n")
  (if electric-indent-inhibit
      ;; We can't use `indent-according-to-mode' in languages like Python,
      ;; as there are multiple possible indentations with different meanings.
      (let* ((indent-end (progn (back-to-indentation) (point)))
             (indent-start (progn (move-beginning-of-line nil) (point)))
             (indent-chars (buffer-substring indent-start indent-end)))
        (forward-line -1)
        ;; This new line should be indented with the same characters as
        ;; the current line.
        (insert indent-chars))
    ;; Just use the current major-mode's indent facility.
    (progn
      (forward-line -1)
      (indent-according-to-mode))))

(defun crux-mini-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (crux-mini-smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(defun crux-mini-top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

(defun crux-mini-kill-whole-line (&optional arg)
  "A simple wrapper around command `kill-whole-line' that respects indentation.
Passes ARG to command `kill-whole-line' when provided."
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))

(defun crux-mini-kill-line-backwards ()
  "Kill line backwards and adjust the indentation."
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))

(defun crux-mini-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun crux-mini-indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(defun crux-mini-rename-file-and-buffer ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((new-name (read-from-minibuffer "New name: " filename))
             (containing-dir (file-name-directory new-name)))
        (make-directory containing-dir t)
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defalias 'crux-mini-rename-buffer-and-file #'crux-mini-rename-file-and-buffer)

(defun crux-mini-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defalias 'crux-mini-delete-buffer-and-file #'crux-mini-delete-file-and-buffer)

(defun crux-mini-transpose-windows (arg)
  "Transpose the buffers shown in two windows.
Prefix ARG determines if the current windows buffer is swapped
with the next or previous window, and the number of
transpositions to execute in sequence."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (cl-plusp arg) (1- arg) (1+ arg))))))

(defalias 'crux-mini-swap-windows 'crux-mini-transpose-windows)

(defun crux-mini-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun crux-mini-find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
