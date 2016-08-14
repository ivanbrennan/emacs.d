;; blank slate
(setq initial-scratch-message nil
      inhibit-startup-screen t
      frame-title-format "emacs")

;; clean screen
(menu-bar-mode   0)
(tool-bar-mode   0)
(scroll-bar-mode 0)
(tooltip-mode    0)

;; useful indicators
(column-number-mode)
(show-paren-mode)
(global-hl-line-mode)

;; cursor
(setq-default cursor-in-non-selected-windows nil)
(blink-cursor-mode 0)

;; theme
(setq custom-theme-directory (locate-user-emacs-file "themes/"))
(make-directory custom-theme-directory :mkdir_p)

(let ((theme 'github))
  (unless (ignore-errors (load-theme theme :no-confirm))
  (message "Unable to find theme file for ‘%s’" theme)))

(let ((font "Source Code Pro-16"))
  (set-face-attribute 'default t :font font)
  (set-frame-font font nil t))

(defun ivan/buffer-face-mode-variable (height)
  "Set font to a variable width font in the current buffer"
  (interactive)
  (setq buffer-face-mode-face `(:family "Avenir Next" :height ,height))
  (buffer-face-mode))

(add-hook 'help-mode-hook
          (apply-partially #'ivan/buffer-face-mode-variable 180))
(add-hook 'Info-mode-hook
          (apply-partially #'ivan/buffer-face-mode-variable 200))

;; transparency
(set-frame-parameter (selected-frame) 'alpha '(97 . 85))
(add-to-list 'default-frame-alist   '(alpha . (97 . 85)))

(defun ivan/toggle-transparency ()
  (interactive)
  (setq frame-alpha-lower-limit
        (if (eql frame-alpha-lower-limit 100) 20 100))
  (set-frame-parameter nil 'alpha (frame-parameter nil 'alpha)))

;; line-wrapping
(setq-default truncate-lines t)
(add-hook 'help-mode-hook #'visual-line-mode)
(add-hook 'Info-mode-hook #'visual-line-mode)

;; splits
(setq split-width-threshold 130)

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
(make-directory (locate-user-emacs-file ".cache/") :mkdir_p)

(setq backup-directory-alist
      `(("." . ,(locate-user-emacs-file ".cache/backups/"))))
(setq savehist-file
      (locate-user-emacs-file ".cache/savehist"))
(setq eshell-directory-name
      (locate-user-emacs-file ".cache/eshell/"))

(setq backup-by-copying t)
(savehist-mode)

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

;; packages
(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/")
             :append)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/")
             :append)

(setq package-archive-priorities '(("melpa-stable" . 2) ("marmalade" . 1)))
(setq package-enable-at-startup nil)

(defun package-path (pkg)
  "Return the path of the highest installed version of package PKG,
or nil if no installed versions are found."
  (let ((name (symbol-name pkg)))
    (car (last
          (file-expand-wildcards
           (locate-user-emacs-file
            (concat "elpa/" name "*/" name ".el")))))))

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

(use-package evil
  :load-path "elpa/evil-1.2.12"
  :commands evil-mode)

(use-package page-break-lines
  :load-path "elpa/page-break-lines-0.11"
  :commands page-break-lines-mode
  :config
  (add-hook 'help-mode-hook 'page-break-lines-mode)
  (add-hook 'Info-mode-hook 'page-break-lines-mode))

;;(use-package magit...
;;(use-package auto-complete...
;;(use-package projectile...
;;(use-package ag...

;; gui & terminal
(defun configure-gui ()
  (bind-key "s-q" 'save-buffers-kill-terminal)
  (bind-key "s-v" 'yank)
  (bind-key "s-c" 'evil-yank)
  (bind-key "s-a" 'mark-whole-buffer)
  (bind-key "s-o" 'find-file)
  (bind-key "s-x" 'kill-region)
  (bind-key "s-w" 'delete-window)
  (bind-key "s-W" 'delete-frame)
  (bind-key "s-n" 'make-frame)
  (bind-key "s-z" 'undo-tree-undo)
  (bind-key "s-Z" 'undo-tree-redo)
  (bind-key "s-s" 'save-buffer)
  ;; turn off "displays have separate spaces" so
  ;; fullscreen won't black out other monitors.
  (bind-key "s-<return>" 'toggle-frame-fullscreen)
  (bind-key "M-s-h" 'mac-hide-others))

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
  (bind-key "<mouse-4>" 'alternating-scroll-down-line)
  (bind-key "<mouse-5>" 'alternating-scroll-up-line)

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

(defun system-is-mac () (eq system-type 'darwin))

(defun configure-mac-modifiers ()
       (setq mac-command-modifier 'super)
       (setq mac-option-modifier 'meta))

(if (system-is-mac)
    (configure-mac-modifiers))

(if (display-graphic-p)
    (configure-gui)
  (configure-terminal))


;; etc.
(defun ivan/goto-match-beginning ()
  (when (and isearch-forward isearch-other-end (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))
(add-hook 'isearch-mode-end-hook 'ivan/goto-match-beginning)

;; let Magit handle Git
(setq vc-handled-backends (delq 'Git vc-handled-backends))


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


;; keybindings
(bind-key "C-<return>" 'crux-mini-smart-open-line)
(bind-key "S-<return>" 'crux-mini-smart-open-line-above)
(global-set-key [remap move-beginning-of-line]
                'crux-mini-move-beginning-of-line)
(bind-key "s-u" 'ivan/toggle-transparency)
(bind-key "M-s-;" 'global-hl-line-mode)
