;; UTF-8 as the default coding system (from Doom)
;; (set-charset-priority 'unicode)        ; could this impact performance?
;; (prefer-coding-system        'utf-8)   ; looks like it's defaulting to utf-8 anyway?
;; (set-terminal-coding-system  'utf-8)   ; looks like it's defaulting to utf-8 anyway?
;; (set-keyboard-coding-system  'utf-8)   ; looks like it's defaulting to utf-8 anyway?
(set-selection-coding-system 'utf-8)
;; (setq locale-coding-system   'utf-8)   ; looks like it's defaulting to utf-8 anyway?
;; (setq-default buffer-file-coding-system 'utf-8) ; looks like it's defaulting to utf-8 anyway?

;; sensibility
(setq
 bookmark-bmenu-toggle-filenames     nil
 bookmark-default-file               (ivan-cache-file "bookmarks")
 comint-prompt-read-only             t
 compilation-always-kill             t
 compilation-ask-about-save          nil
 compilation-message-face            nil  ; don't underline compilation links
 compilation-save-buffers-predicate  (lambda () (eq (window-buffer) (current-buffer)))
 completions-format                  'vertical
 dabbrev-case-fold-search            nil
 ediff-split-window-function         #'split-window-horizontally
 ediff-window-setup-function         'ediff-setup-windows-plain
 hi-lock-auto-select-face            t
 history-delete-duplicates           t
 history-length                      500
 ispell-program-name                 "/run/current-system/sw/bin/aspell"
 minibuffer-eldef-shorten-default    t
 query-replace-skip-read-only        t
 read-buffer-completion-ignore-case  t
 sentence-end-double-space           nil
 split-window-keep-point             nil
 tab-always-indent                   'complete
 vc-follow-symlinks                  t
 )

(fset 'yes-or-no-p 'y-or-n-p)

(defalias 'list-buffers 'ibuffer)
(minibuffer-electric-default-mode +1)
(delete-selection-mode +1)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'tex-mode-hook #'(lambda () (setq ispell-parser 'tex)))

(defun ivan-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory ‘%s’ does not exist! Create it?" parent-directory)))
      (make-directory parent-directory 'mkdir_p))))

(add-to-list 'find-file-not-found-functions
             'ivan-create-non-existent-directory)


;; env
(setenv "PAGER" "/usr/bin/env cat")


;; tramp
(setq tramp-default-method "ssh")


(use-package async
  :config
  (async-bytecomp-package-mode 1))

(use-package doom-themes
  :pin melpa
  :config
  (setq
   doom-enable-bold   t
   doom-enable-italic t
   ivan-want-brighten-minibuffer t
   )
  (setq
   doom-neotree-enable-variable-pitch t
   doom-neotree-file-icons 'simple
   )

  (with-eval-after-load 'face-remap
    ;; doom-buffer-mode marks face-remapping-alist permanent-local.
    ;; don't let that interfere with consistent text-scale-mode behavior.
    (mapc (lambda (x) (put x 'permanent-local t))
          '(text-scale-mode-remapping
            text-scale-mode-lighter
            text-scale-mode-amount
            buffer-face-mode-remapping)))

  (defun ivan-update-doom-settings ()
    (if (or (memq 'dome   custom-enabled-themes)
            (memq 'elixir custom-enabled-themes))
        (ivan-activate-doom-config)
      (ivan-deactivate-doom-config))
    (powerline-reset))

  (defvar ivan-doomable-mode-hooks '(find-file-hook
                                     Info-mode-hook
                                     splat-buffer-mode-hook
                                     ediff-prepare-buffer-hook))

  (defun ivan-doomable-buffer? ()
    (or buffer-file-name
        (derived-mode-p 'prog-mode)
        (derived-mode-p 'Info-mode)
        (equal "*splat*"  (buffer-name))))

  (defun ivan-doom-buffer-mode-maybe ()
    "Enable `doom-buffer-mode' in the current buffer, if it isn't already and the
buffer represents a real file."
    (when (and (not doom-buffer-mode)
               (ivan-doomable-buffer?))
      (doom-buffer-mode +1)))

  (defun ivan-activate-doom-config ()
    (dolist (hook ivan-doomable-mode-hooks)
      (add-hook hook #'ivan-doom-buffer-mode-maybe))
    (when ivan-want-brighten-minibuffer
      (add-hook 'minibuffer-setup-hook #'doom-brighten-minibuffer))
    (mapc #'ivan-specific-buffer-doom-buffer-mode-maybe (buffer-list)))

  (defun ivan-specific-buffer-doom-buffer-mode-maybe (buffer)
    (with-current-buffer buffer
      (let ((name (buffer-name buffer)))
        (unless (and (string-match-p "^[ *]" name)
                     (not (equal "*splat*"  name)))
          (ivan-doom-buffer-mode-maybe)))))

  (defun ivan-deactivate-doom-config ()
    (dolist (hook ivan-doomable-mode-hooks)
      (remove-hook hook #'ivan-doom-buffer-mode-maybe))
    (remove-hook 'minibuffer-setup-hook #'doom-brighten-minibuffer)
    (doom-buffer-mode 0))

  (add-hook 'ivan-rotated-theme-hook #'ivan-update-doom-settings)
  (add-hook 'after-init-hook #'ivan-update-doom-settings)

  (when (display-graphic-p) (doom-themes-neotree-config))

  (defun ivan-adjust-fringe-background (n)
    (let* ((default-bg (face-background 'default))
           (doom-bg (or (face-background 'doom-default) default-bg))
           (bg (if (and (> n ivan-padding-min) (not doom-buffer-mode))
                   default-bg
                 doom-bg)))
      (set-face-background 'fringe bg)))
  (advice-add 'ivan--apply-padding-degree :after #'ivan-adjust-fringe-background))

(use-package nix-mode
  :defer t)

(use-package neotree
  :config
  (setq neo-mode-line-type 'none)
  (add-hook 'neotree-mode-hook
            (lambda ()
              (setq-local evil-motion-state-cursor
                          '((bar . 1)
                            (lambda ()
                              (evil-set-cursor-color (face-foreground 'mode-line)))))))

  (defmacro doom/neotree-save (&rest body)
    `(let ((neo-p (neo-global--window-exists-p)))
       (when neo-p (neotree-hide))
       ,@body
       (when neo-p
         (save-selected-window
           (neotree-show)))))

  (defun doom*save-neotree (orig-fun &rest args)
    "Prevents messing up the neotree buffer on window changes"
    (doom/neotree-save (apply orig-fun args)))

  (advice-add 'balance-windows              :around 'doom*save-neotree)
  (advice-add 'evil-window-move-very-bottom :around 'doom*save-neotree)
  (advice-add 'evil-window-move-very-top    :around 'doom*save-neotree)
  (advice-add 'evil-window-move-far-left    :around 'doom*save-neotree)
  (advice-add 'evil-window-move-far-right   :around 'doom*save-neotree)
  )

(use-package f
  :commands (f-dirname f-relative))

(use-package evil
  :pin melpa
  :demand
  :bind
  (:map evil-normal-state-map
        ("U"           . undo-tree-redo)
        ("C-r"         . isearch-backward)
        ("S-SPC"       . ivan-emacs-state-rectangle-mark-mode)
        ("C-<return>"  . ivan-add-whitespace-below)
        ("S-<return>"  . ivan-add-whitespace-above)
        ("C-m"         . evil-next-line-first-non-blank)
        ("C-t"         . transpose-words)
        ("M-t"         . transpose-sexps)
        ("+"           . evil-numbers/inc-at-pt)
        ("-"           . evil-numbers/dec-at-pt)
        ("t"           . ivan-run-tests-or-find-char-to)
        ("T"           . ivan-run-test-file-or-find-char-to-backward)
        ("S-SPC"       . evil-ex)
        ("z q"         . ivan-quit-no-confirm)
        :map evil-motion-state-map
        ("C-d"         . ivan-kill-buffer-and-maybe-window)
        ("C-e"         . evil-end-of-line)
        ("C-j"         . evil-scroll-line-down)
        ("C-k"         . evil-scroll-line-up)
        ("M-l"         . goto-line)
        ("C-w C-h"     . evil-window-left)
        ("C-w C-j"     . evil-window-down)
        ("C-w C-k"     . evil-window-up)
        ("C-w C-l"     . evil-window-right)
        ("C-w s"       . split-window-below)
        ("C-w C-s"     . split-window-below)
        ("C-w SPC"     . ivan-zoom-window)
        ("C-w C-SPC"   . ivan-zoom-window)
        ("C-w S-SPC"   . ivan-zoom-other-window)
        ("M-S-SPC"     . eval-expression)
        ("M-<left>"    . evil-beginning-of-line)
        ("M-<right>"   . evil-end-of-line)
        ("S-<left>"    . left-word)
        ("S-<right>"   . right-word)
        ("˜"           . next-error)
        ("Δ"           . next-error)
        ("∏"           . previous-error)
        ("˚"           . previous-error)
        :map evil-visual-state-map
        ("<return>"    . evil-next-line-first-non-blank)
        ("C-m"         . evil-next-line-first-non-blank)
        ("C-r"         . isearch-backward)
        ("<tab>"       . evil-indent)
        ("<backspace>" . delete-active-region)
        :map evil-insert-state-map
        ("M-v"         . yank)
        ("C-S-U"       . ivan-backward-kill-line)
        ("C-<return>"  . evil-open-below)
        ("S-<return>"  . evil-open-above)
        ("C-d"         . nil)
        ("C-t"         . nil)
        :map evil-replace-state-map
        ("M-v"         . yank)
        ("C-e"         . evil-copy-from-below)
        ("C-y"         . evil-copy-from-above)
        :map evil-ex-completion-map
        ("C-a"         . move-beginning-of-line)
        )
  :config
  (progn
    (add-hook 'after-init-hook #'evil-mode)
    (with-eval-after-load 'savehist
      (add-to-list 'savehist-additional-variables 'evil-ex-history))
    (setq
     evil-normal-state-tag   " ·n·"
     evil-visual-state-tag   " ·v·"
     evil-operator-state-tag " ·o·"
     evil-motion-state-tag   " ·m·"
     evil-insert-state-tag   " ·i·"
     evil-replace-state-tag  " ·r·"
     evil-emacs-state-tag    " ·e·"
     )
    (mapc (lambda (r) (evil-set-initial-state (car r) (cdr r)))
          '((compilation-mode       . motion)
            (help-mode              . motion)
            (grep-mode              . motion)
            (ag-mode                . motion)
            (Info-mode              . motion)
            (bookmark-bmenu-mode    . motion)
            (ibuffer-mode           . motion)
            (imenu-list-major-mode  . motion)
            (neotree-mode           . motion)
            (message-mode           . normal)
            (debugger-mode          . normal)
            (image-mode             . normal)
            (doc-view-mode          . normal)
            (eww-mode               . normal)
            (tabulated-list-mode    . normal)
            (profile-report-mode    . emacs)
            (comint-mode            . emacs)
            (cider-repl-mode        . emacs)
            (term-mode              . emacs)
            (calendar-mode          . emacs)
            (Man-mode               . emacs)))

    (defun ivan-adjust-view-mode-evil-state ()
      (if view-mode (evil-motion-state) (evil-initialize-state)))
    (add-hook 'view-mode-hook #'ivan-adjust-view-mode-evil-state)

    (setq
     evil-insert-state-cursor '(bar . 1)
     evil-emacs-state-cursor  'bar
     evil-default-cursor '(t (lambda ()
                               (evil-set-cursor-color
                                (face-foreground 'minibuffer-prompt))))
     )
    (setq evil-ex-visual-char-range t)
    (setq-default
     evil-shift-width 2
     evil-symbol-word-search t
     )
    (customize-set-variable 'evil-want-Y-yank-to-eol t)

    (mapc 'evil-declare-ignore-repeat
          '(describe-key-briefly))

    (defun ivan-trim-autoinserted-whitespace ()
      (when (memq last-command
                  '(evil-open-above
                    evil-open-below
                    evil-append
                    evil-append-line
                    newline
                    newline-and-indent
                    indent-and-newline))
        (add-hook 'post-command-hook #'evil-maybe-remove-spaces)
        (setq evil-maybe-remove-spaces t)))
    (add-hook 'evil-insert-state-exit-hook #'ivan-trim-autoinserted-whitespace)

    (defun ivan-maybe-trim-whitespace-current-line (&rest _args)
      (when (evil-insert-state-p) (ivan-trim-whitespace-current-line)))
    (advice-add 'evil-open-below :before #'ivan-maybe-trim-whitespace-current-line)
    (advice-add 'evil-open-above :before #'ivan-maybe-trim-whitespace-current-line)

    (evil-define-command ivan-paste-pop-or-previous-line (count)
      :repeat nil
      (interactive "p")
      (let ((paste (memq last-command '(evil-paste-after
                                        evil-paste-before
                                        evil-visual-paste))))
        (setq this-command (if paste
                               'evil-paste-pop
                             'evil-previous-line-first-non-blank)))
      (funcall this-command count))

    (evil-define-command ivan-paste-pop-or-next-line (count)
      :repeat nil
      (interactive "p")
      (ivan-paste-pop-or-previous-line (- count)))

    (define-key evil-normal-state-map "\C-n" #'ivan-paste-pop-or-next-line)
    (define-key evil-normal-state-map "\C-p" #'ivan-paste-pop-or-previous-line)

    (defun ivan-visual-shift-left ()
      (interactive)
      (call-interactively #'evil-shift-left)
      (ivan-visual-restore))
    (defun ivan-visual-shift-right ()
      (interactive)
      (call-interactively #'evil-shift-right)
      (ivan-visual-restore))
    (defsubst ivan-visual-restore ()
      (evil-normal-state)
      (evil-visual-restore))
    (define-key evil-visual-state-map (kbd "<") #'ivan-visual-shift-left)
    (define-key evil-visual-state-map (kbd ">") #'ivan-visual-shift-right)

    (defun ivan-run-tests-or-find-char-to (count char)
      (interactive "p\nc")
      (if (= char ?\r)
          (ivan-run-test)
        (evil-find-char-to count char)))

    (defun ivan-run-test ()
      (cond
       ((eq 'ruby-mode major-mode)
        (ivan-rspec-dwim 'rspec-verify-single))
       ((eq 'rspec-compilation-mode major-mode)
        (rspec-rerun))
       ((eq 'emacs-lisp-mode major-mode)
        (ivan-ert))))

    (defun ivan-ert ()
      (let ((original-window (selected-window))
            (original-frame (selected-frame)))
        (ert-run-tests-interactively t)
        (select-frame-set-input-focus original-frame 'no-record)
        (select-window original-window 'no-record)))

    (defun ivan-run-test-file-or-find-char-to-backward (count char)
      (interactive "p\nc")
      (if (= char ?\r)
          (ivan-rspec-dwim 'rspec-verify)
        (evil-find-char-to-backward count char)))

    (defun ivan-substitute-word-on-line ()
      (interactive)
      (minibuffer-with-setup-hook
          (lambda () (backward-char 2))
        (evil-ex (format "s/%s//g" (thing-at-point 'word 'no-properties)))))

    (defun ivan-substitute-word-in-buffer ()
      (interactive)
      (minibuffer-with-setup-hook
          (lambda () (backward-char 2))
        (evil-ex (format "%%s/%s//g" (thing-at-point 'word 'no-properties)))))

    (defun ivan-emacs-state-rectangle-mark-mode ()
      (interactive)
      (evil-emacs-state)
      (rectangle-mark-mode +1))
    (defun ivan-backward-kill-line () (kill-line 0))
    (evil-define-key 'motion apropos-mode-map  (kbd "<tab>") #'forward-button)
    (evil-define-key 'motion help-mode-map     (kbd "<tab>") #'forward-button)
    (evil-define-key 'motion help-mode-map     (kbd "C-o")   #'help-go-back)
    (evil-define-key 'motion help-mode-map     (kbd "C-i")   #'help-go-forward)
    (evil-define-key 'motion help-mode-map     (kbd "M-S-<return>") #'ivan-zoom-window-large)
    (evil-define-key 'motion apropos-mode-map  (kbd "M-S-<return>") #'ivan-zoom-window-large)
    (evil-define-key 'motion neotree-mode-map  (kbd "u")     #'neotree-hidden-file-toggle)
    (evil-define-key 'motion neotree-mode-map  (kbd "<escape>") #'neotree-hide)
    (evil-define-key 'normal debugger-mode-map (kbd "q")     #'top-level)

    (require 'core-hydra-dired-preview)
    (evil-define-key 'normal dired-mode-map "." #'hydra-dired-preview/dired-preview-current)

    (setq evil-want-C-i-jump nil) ;; don't clobber TAB in terminal
    (define-key evil-motion-state-map [C-i] #'evil-jump-forward) ;; GUI only
    (define-key evil-normal-state-map (kbd "M-.") nil)
    (evil-define-key 'normal ggtags-mode-map (kbd "M-.") #'ggtags-find-tag-dwim)

    (defun ivan-move-key (keymap-from keymap-to key)
      "Moves key binding from one keymap to another, deleting from the old location."
      (define-key keymap-to key (lookup-key keymap-from key))
      (define-key keymap-from key nil))
    (ivan-move-key evil-motion-state-map evil-normal-state-map (kbd "RET")))

  (defun doom--point-at-bol-non-blank()
    (save-excursion (evil-first-non-blank) (point)))

  (defun doom/backward-kill-to-bol-and-indent ()
    "Kill line to the first non-blank character. If invoked again \
afterwards, kill line to column 1."
    (interactive)
    (let ((empty-line (sp-point-in-blank-line)))
      (evil-delete (point-at-bol) (point))
      (if (not empty-line)
          (indent-according-to-mode))))

  (define-key evil-insert-state-map (kbd "C-u") #'doom/backward-kill-to-bol-and-indent)

  (evil-define-command ivan-window-mru ()
    "Switch to the most recently used window in the current frame."
    :repeat nil
    (let ((win (get-mru-window nil 'dedicated-ok 'not-selected)))
      (when win (select-window win))))

  (define-key evil-motion-state-map [remap evil-window-mru] #'ivan-window-mru)

  (defun ivan-guess-alternate-buffer (&rest _args)
    (let* ((buf (other-buffer))
           (pos (with-current-buffer buf (point))))
      (list buf 0 pos)))
  (advice-add 'evil-alternate-buffer :after-until #'ivan-guess-alternate-buffer))

(use-package evil-goggles
  :config
  (setq evil-goggles-default-face 'lazy-highlight
        evil-goggles-duration     0.100)
  (evil-goggles-mode))

(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :commands elisp-slime-nav-mode
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode))
  :config
  (progn
    (evil-define-key 'normal emacs-lisp-mode-map
      (kbd "M-.") #'elisp-slime-nav-find-elisp-thing-at-point)
    (evil-define-key 'normal ielm-map
      (kbd "M-.") #'elisp-slime-nav-find-elisp-thing-at-point)
    )
  )

(use-package auto-compile
  :disabled t
  :commands auto-compile-on-save-mode
  :init (add-hook 'emacs-lisp-mode-hook 'auto-compile-on-save-mode)
  :config (setq auto-compile-display-buffer nil))

(use-package ggtags
  :commands (ggtags-mode
             ivan-ggtags-maybe
             ivan-dired-ggtags-maybe
             ggtags-navigation-mode
             ggtags-find-tag-dwim
             ggtags-find-tag-regexp)
  :diminish ggtags-mode
  :init
  (add-hook 'prog-mode-hook #'ivan-ggtags-maybe)
  (with-eval-after-load 'org (add-hook 'org-mode-hook #'ggtags-mode))
  (dolist (hook '(yaml-mode-hook))
    (add-hook hook #'ggtags-mode))
  (add-hook 'dired-mode-hook #'ivan-dired-ggtags-maybe)
  (defun ivan-check-tags-state ()
    (interactive)
    (let ((project (and (bound-and-true-p ggtags-mode)
                        (ggtags-find-project))))
      (when project
        (message (concat "Tags: "
                         (if (ggtags-project-dirty-p project)
                             "updating..."
                           "up-to-date"))))))
  :config
  (customize-set-variable 'ggtags-highlight-tag nil)
  (defun ivan-dired-ggtags-maybe ()
    (when (locate-dominating-file default-directory ".git") (ggtags-mode +1)))
  (defun ivan-ggtags-maybe ()
    (let ((no-ggtags-modes '(emacs-lisp-mode)))
      (unless (memq major-mode no-ggtags-modes)
        (ggtags-mode +1))))
  (defun ivan-add-ggtags-presenter ()
    (add-hook 'compilation-finish-functions #'ivan-present-search-results))
  (add-hook 'ggtags-global-mode-hook #'ivan-compilation-start-at-first-error)
  (add-hook 'ggtags-global-mode-hook #'ivan-add-ggtags-presenter)

  (define-key ggtags-navigation-map (kbd "M-o") nil)
  )

(defun ivan-compilation-start-at-first-error ()
  (set (make-local-variable 'compilation-scroll-output) 'first-error))

(defvar next-error-target-window)
(make-variable-buffer-local 'next-error-target-window)

(advice-add 'compilation-goto-locus :around #'ivan-around-compilation-goto-locus)

(defun ivan-around-compilation-goto-locus (orig-fun &rest args)
  (advice-add 'display-buffer :around #'ivan-handle-next-error-display)
  (advice-add 'pop-to-buffer :override #'ivan-pop-to-buffer)
  (apply orig-fun args)
  (advice-remove 'display-buffer #'ivan-handle-next-error-display)
  (advice-remove 'pop-to-buffer #'ivan-pop-to-buffer))

(defun ivan-handle-next-error-display (orig-fun &rest args)
  (advice-remove 'display-buffer #'ivan-handle-next-error-display)
  (if (memq this-command '(next-error previous-error))
      (get-buffer-window (car args) 0)
    (apply orig-fun args)))

(defun ivan-pop-to-buffer (buffer &optional action norecord)
  (advice-remove 'pop-to-buffer #'ivan-pop-to-buffer)
  (let ((old-frame (selected-frame))
        (window (ivan-display-buffer-in-existing-window
                 buffer 'next-error-target-window)))
    (if window
        (select-window window norecord)
      (pop-to-buffer buffer action norecord)
      (with-current-buffer next-error-last-buffer
        (setq-local next-error-target-window (selected-window))))
    (unless (eq old-frame (window-frame window))
      (select-frame-set-input-focus (window-frame window) norecord)))
  buffer)

(defun ivan-display-buffer-in-existing-window (buffer window-var)
  (or (display-buffer-reuse-window buffer nil)
      (ivan-display-buffer-try-window buffer window-var)))

(defun ivan-display-buffer-try-window (buffer window-var)
  (when (and (boundp window-var) (window-live-p (symbol-value window-var)))
    (window--display-buffer buffer (symbol-value window-var) 'reuse)))

(use-package yasnippet
  :defer t)

(use-package nand2tetris
  :disabled t
  :ensure nil
  :mode ("\\.hdl\\'" . nand2tetris-mode)
  :config
  (setq nand2tetris-1 1))

(use-package rainbow-mode
  :diminish rainbow-mode
  :commands rainbow-mode
  :init
  (progn
    (defun ivan-colorize-theme ()
      (if (string-match "-theme.el$" (buffer-name)) (rainbow-mode +1)))
    (add-hook 'emacs-lisp-mode-hook #'ivan-colorize-theme))
  :config
  (add-hook 'rainbow-mode-hook (lambda () (hl-line-mode 0))))

(use-package highlight-numbers
  :commands highlight-numbers-mode
  :init
  (dolist (hook '(c-mode-hook
               css-mode-hook
               emacs-lisp-mode-hook
               sh-mode-hook))
    (add-hook hook #'highlight-numbers-mode)))

(use-package highlight-quoted
  :init
  (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode))

(use-package highlight-parentheses
  :commands highlight-parentheses-mode)

(use-package hl-line
  :config
  (defface bold-hl-line
    '((t (:weight bold :inherit hl-line)))
    "Bold hl-line face.")

  (defun enable-bold-hl-line ()
    (setq-local face-remapping-alist '((hl-line . bold-hl-line)))
    (hl-line-mode +1))

  (add-hook 'dired-mode-hook
            (lambda ()
              (enable-bold-hl-line)
              (setq-local evil-normal-state-cursor '(bar . 0))))

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (enable-bold-hl-line)
              (setq-local evil-motion-state-cursor '(bar . 0))))

  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'bookmark-bmenu-mode-hook
            (lambda ()
              (enable-bold-hl-line)
              (setq-local evil-motion-state-cursor '(bar . 0))))

  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)
)

(use-package elixir-mode
  :config
  (use-package alchemist))

(use-package undo-tree
  :diminish undo-tree-mode
  :commands (undo-tree-undo undo-tree-redo)
  :bind (:map undo-tree-map ("M-_" . nil))
  :config
  (progn
    (setq undo-tree-history-directory-alist
          `(("." . ,(ivan-cache-file "undo-tree-history/"))))
    (defun undo-tree-visualizer-update-linum (start end old-len)
      (if (fboundp 'linum-update)
          (linum-update undo-tree-visualizer-parent-buffer)))
    (add-hook 'undo-tree-visualizer-mode-hook
              (lambda ()
                (add-hook 'after-change-functions
                          #'undo-tree-visualizer-update-linum nil :local)))
    (undo-tree-mode +1)))

(use-package zoom-window
  :commands (zoom-window-zoom zoom-window--enable-p)
  :config
  (setq
   zoom-window-mode-line-color (face-background 'header-line)
   )
  (defun ivan-maybe-reset-zoom (&rest _)
    (when (or (derived-mode-p 'help-mode)
              (derived-mode-p 'apropos-mode))
      (text-scale-set 0)
      (when (zoom-window--enable-p)
        (zoom-window-zoom))))

  (mapc (lambda (x) (advice-add x :before #'ivan-maybe-reset-zoom))
        '(quit-window
          evil-window-delete
          kill-buffer-and-window
          delete-other-windows))
  )

(defun ivan-zoom-window ()
  (interactive)
  (let ((window-side (window-parameter nil 'window-side)))
    (when window-side (set-window-parameter nil 'window-side nil))
    (zoom-window-zoom)
    (when window-side (set-window-parameter nil 'window-side window-side)))
  (powerline-reset))

(defun ivan-zoom-other-window ()
  (interactive)
  (if (zoom-window--enable-p)
      (progn
        (ivan-zoom-window)
        (other-window -1))
    (other-window 1)
    (ivan-zoom-window)))

(defun ivan-zoom-window-large ()
  (interactive)
  (ivan-zoom-window)
  (text-scale-set (if (zoom-window--enable-p) 1 0)))

(defun ivan-toggle-narrowing-zoom (p)
  (interactive "P")
  (ivan-zoom-window)
  (ivan-toggle-narrowing p))

(defun ivan-toggle-narrowing (p)
  (interactive "P")
  (cond
   ((and (buffer-narrowed-p) (not p))
    (widen))
   ((use-region-p)
    (narrow-to-region (region-beginning) (region-end)))
   (t
    (narrow-to-defun))))

(defun ivan-redraw-display ()
  (interactive)
  (redraw-display)
  (when (bound-and-true-p nlinum-mode)
    (nlinum--flush)))

(use-package workgroups2
  ;; TODO: investigate persp-mode as an alternative
  :when (display-graphic-p)
  :commands (wg-open-session workgroups-mode)
  :init
  (make-directory (ivan-cache-file "workgroups") 'mkdir_p)
  (setq
   wg-session-file (ivan-cache-file "workgroups/last")
   wg-first-wg-name "*untitled*"
   wg-session-load-on-start nil
   wg-mode-line-display-on nil
   wg-mess-with-buffer-list nil
   wg-emacs-exit-save-behavior 'save
   wg-workgroups-mode-exit-save-behavior 'save
   wg-log-level 0)
  (advice-add 'wg-change-modeline :override 'ignore)

  (defun doom|wg-cleanup ()
    "Remove unsavable windows and buffers before we save the window
configuration."
    (let (splat-buffer-inhibit-refresh)
      ;; (doom/popup-close-all)
      (when (and (featurep 'neotree) (neo-global--window-exists-p))
        (neotree-hide))))
  (add-hook 'kill-emacs-hook 'doom|wg-cleanup)

  (workgroups-mode +1)
  :config
  (defun ivan-current-workgroup-name ()
    (let ((wg (ignore-errors (wg-current-workgroup))))
      (when (wg-current-workgroup-p wg)
        (wg-workgroup-name wg))))

  (with-eval-after-load 'hydra
    (defun ivan-layout-name ()
      (let ((name (ivan-current-workgroup-name)))
        (propertize (or name "") 'face 'font-lock-variable-name-face)))
    (defun ivan-layout-label ()
      (propertize "Layout:" 'face 'hydra-face-title))

    (defhydra hydra-layouts (:exit t
                             :hint nil
                             :idle 0.5)
      "
 %s(ivan-layout-label) %s(ivan-layout-name)
 _n_: next       _l_: last       _c_: create     _m_: rename
 _p_: previous   _s_: select     _k_: kill       _q_: quit
      "
      ("n"        wg-switch-to-workgroup-right :exit nil)
      ("p"        wg-switch-to-workgroup-left :exit nil)
      ("l"        wg-switch-to-previous-workgroup)
      ("s"        wg-switch-to-workgroup)
      ("SPC"      wg-switch-to-workgroup)
      ("c"        wg-create-workgroup)
      ("m"        wg-rename-workgroup)
      ("k"        wg-kill-workgroup)
      ("q"        nil)
      ("<escape>" nil)))
)

(use-package macrostep
  :commands macrostep-expand
  :init
  (with-eval-after-load 'hydra
    (defhydra hydra-macrostep (:color red)
      "macrostep"
      ("e"        macrostep-expand "expand")
      ("c"        macrostep-collapse "collapse")
      ("n"        macrostep-next-macro "next")
      ("p"        macrostep-prev-macro "previous")
      ("q"        macrostep-collapse-all "quit" :color blue)
      ("<escape>" macrostep-collapse-all "quit" :color blue))))

(use-package smartparens
  :diminish smartparens-mode
  :init
  (add-hook 'prog-mode-hook #'smartparens-strict-mode)
  :config
  (require 'smartparens-config)
  (global-set-key (kbd "M-)") #'sp-forward-slurp-sexp)
  (global-set-key (kbd "M-(") #'sp-forward-barf-sexp)
  (global-set-key (kbd "C-(") #'sp-backward-slurp-sexp)
  (global-set-key (kbd "C-)") #'sp-backward-barf-sexp)

  (with-eval-after-load 'evil
    (add-hook 'evil-replace-state-entry-hook 'turn-off-smartparens-mode)
    (add-hook 'evil-replace-state-exit-hook  'turn-on-smartparens-mode)
    (evil-define-key 'visual smartparens-mode-map
      "(" (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "(")))
    (evil-define-key 'visual smartparens-mode-map
      "[" (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "[")))
    (evil-define-key 'visual smartparens-mode-map
      "{" (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "{")))
    (evil-define-key 'visual smartparens-mode-map (kbd "<return>") #'sp-unwrap-sexp)
    (evil-define-key 'insert smartparens-mode-map (kbd "M-)") #'sp-forward-slurp-sexp)
    (evil-define-key 'insert smartparens-mode-map (kbd "M-(") #'sp-forward-barf-sexp)
    (evil-define-key 'insert smartparens-mode-map (kbd "C-(") #'sp-backward-slurp-sexp)
    (evil-define-key 'insert smartparens-mode-map (kbd "C-)") #'sp-backward-barf-sexp))

  ;; doom whitespace
  (defun doom/surrounded-p ()
    (and (looking-back "[[{(]\\(\s+\\|\n\\)?\\(\s\\|\t\\)*")
         (let* ((whitespace (match-string 1))
                (match-str (concat whitespace (match-string 2) "[])}]")))
           (looking-at-p match-str))))

  (defun doom/inflate-space-maybe ()
    "Checks if point is surrounded by {} [] () delimiters and adds a
space on either side of the point if so."
    (interactive)
    (if (doom/surrounded-p)
        (progn (call-interactively 'self-insert-command)
               (save-excursion (call-interactively 'self-insert-command)))
      (call-interactively 'self-insert-command)))

  (defun doom/backward-delete-whitespace-to-column ()
    "Delete back to the previous column of whitespace, or as much whitespace as
possible, or just one char if that's not possible."
    (interactive)
    (let* ((context (sp--get-pair-list-context 'navigate))
           (open-pair-re (sp--get-opening-regexp context))
           (close-pair-re (sp--get-closing-regexp context))
           open-len close-len)
      (cond ;; When in strings (sp acts weird with quotes; this is the fix)
       ;; Also, skip closing delimiters
       ((and (and (sp--looking-back open-pair-re)
                  (setq open-len (- (match-beginning 0) (match-end 0))))
             (and (looking-at close-pair-re)
                  (setq close-len (- (match-beginning 0) (match-end 0))))
             (string= (plist-get (sp-get-thing t) :op)
                      (plist-get (sp-get-thing) :cl)))
        (delete-backward-char open-len)
        (delete-char close-len))
       ;; Delete up to the nearest tab column IF only whitespace between
       ;; point and bol.
       ((save-match-data (looking-back "^[\\t ]*" (line-beginning-position)))
        (let ((movement (% (current-column) tab-width))
              (p (point)))
          (when (= movement 0)
            (setq movement tab-width))
          (save-match-data
            (if (string-match "\\w*\\(\\s-+\\)$"
                              (buffer-substring-no-properties (- p movement) p))
                (delete-backward-char (- (match-end 1) (match-beginning 1)))
              (call-interactively 'delete-backward-char)))))
       ;; Otherwise do a regular delete
       (t (call-interactively 'delete-backward-char)))))

  (defun doom/deflate-space-maybe ()
    "Checks if point is surrounded by {} [] () delimiters, and deletes
spaces on either side of the point if so. Resorts to
`doom/backward-delete-whitespace-to-column' otherwise."
    (interactive)
    (save-match-data
      (if (doom/surrounded-p)
          (let ((whitespace-match (match-string 1)))
            (cond ((not whitespace-match)
                   (call-interactively 'delete-backward-char))
                  ((string-match "\n" whitespace-match)
                   (evil-delete (point-at-bol) (point))
                   (call-interactively 'delete-backward-char)
                   (save-excursion (call-interactively 'delete-char)))
                  (t (just-one-space 0))))
        (doom/backward-delete-whitespace-to-column))))

  (bind-key "SPC" #'doom/inflate-space-maybe evil-insert-state-map)
  (define-key evil-insert-state-map [remap delete-backward-char] #'doom/deflate-space-maybe)
  (define-key evil-insert-state-map [remap sp-backward-delete-char] #'doom/deflate-space-maybe))

(use-package evil-multiedit
  :commands (evil-multiedit-match-all
             evil-multiedit-match-and-next
             evil-multiedit-match-and-prev
             evil-multiedit-restore
             evil-multiedit-toggle-or-restrict-region
             evil-multiedit-next
             evil-multiedit-prev
             evil-multiedit-ex-match)
  :init
  (eval-after-load "evil"
    '(progn
       ;; Highlights all matches of the selection in the buffer.
       (define-key evil-visual-state-map "R" 'evil-multiedit-match-all)

       ;; Match the word under cursor (i.e. make it an edit region). Consecutive presses will
       ;; incrementally add the next unmatched match.
       (define-key evil-normal-state-map (kbd "M-d") 'evil-multiedit-match-symbol-and-next)
       ;; Match selected region.
       (define-key evil-visual-state-map (kbd "M-d") 'evil-multiedit-match-and-next)

       ;; Same as M-d but in reverse.
       (define-key evil-normal-state-map (kbd "M-D") 'evil-multiedit-match-symbol-and-prev)
       (define-key evil-visual-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)

       ;; Restore the last group of multiedit regions.
       (define-key evil-visual-state-map (kbd "C-M-D") 'evil-multiedit-restore)

       ;; Ex command that allows you to invoke evil-multiedit with a regular expression, e.g.
       (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)
       )
    )
  :config
  (progn
    ;; RET will toggle the region under the cursor
    (define-key evil-multiedit-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

    ;; For moving between edit regions
    (define-key evil-multiedit-state-map (kbd "C-n") 'evil-multiedit-next)
    (define-key evil-multiedit-state-map (kbd "C-p") 'evil-multiedit-prev)
    (define-key evil-multiedit-insert-state-map (kbd "C-n") 'evil-multiedit-next)
    (define-key evil-multiedit-insert-state-map (kbd "C-p") 'evil-multiedit-prev)
    )
  )

(use-package expand-region
  :commands er/expand-region
  :bind
  (:map evil-visual-state-map
        ("C-SPC" . er/expand-region)))

(use-package evil-numbers
  :commands (evil-numbers/inc-at-pt
             evil-numbers/dec-at-pt))

(use-package evil-matchit
  :init (global-evil-matchit-mode +1))

(use-package bind-map
  :config
  (bind-map ivan-leader-map
    :evil-keys ("SPC")
    :evil-states (normal motion visual)
    :override-minor-modes t)
  (bind-map-set-keys ivan-leader-map
    ","        #'ivan-window-mru
    "`"        #'variable-pitch-mode
    "="        #'align-regexp
    "8"        #'switch-to-splat-buffer
    "9"        #'highlight-parentheses-mode
    "1"        #'shell-command
    "TAB"      #'neotree-toggle
    "<return>" #'toggle-frame-fullscreen
    "q"        #'hydra-smartparens/body
    "C-l"      #'ivan-redraw-display
    "C-n"      #'ivan-toggle-narrowing
    ;; "C-r"      #'ripgrep-regexp
    "C-u"      #'hl-line-mode
    "X SPC"    #'server-edit
    "X s"      #'server-start
    "\\"       #'ivan-next-theme
    "\|"       #'ivan-previous-theme
    "C-b"      #'bury-buffer
    "B"        #'unbury-buffer
    ;; "b ."    #'hydra-buffers/body
    "b i"      #'ibuffer
    "b m j"    #'bookmark-jump
    "b m l"    #'bookmark-bmenu-list
    "b m m"    #'bookmark-set
    "d d"      #'kill-this-buffer
    "f a"      #'find-alternate-file
    "f s"      #'save-buffer
    "f w"      #'write-file
    "g"        #'hydra-layouts/body
    "u"        #'hydra-dir-navigate/dired-jump
    "i"        #'os-switch-to-term
    "a"        #'ag-project
    "C-a"      #'ag-project-regexp
    "'"        #'evil-use-register
    "l"        #'evil-switch-to-windows-last-buffer
    "m e b"    #'eval-buffer
    "m e f"    #'eval-defun
    "m e e"    #'pp-eval-last-sexp
    "m e r"    #'eval-region
    "m d"      #'mark-defun
    "m L"      #'lisp-interaction-mode
    "m m"      #'hydra-macrostep/body
    "o"        #'find-file
    "L"        #'switch-to-buffer
    "s"        #'ivan-substitute-word-on-line
    "S"        #'ivan-substitute-word-in-buffer
    "C-s"      search-map
    "y"        #'ivan-check-tags-state
    "C-v"      #'magit-blame
    "v SPC"    #'ivan-vc-refresh-state
    "b SPC"    #'magit-branch-popup
    "w k"      #'evil-window-delete
    "."        #'ivan-zoom-window
    "C-."      #'ivan-toggle-narrowing-zoom
    "w w"      #'ivan-zoom-other-window
    "w c"      #'evil-window-delete
    "w J"      #'webjump
    "w n"      #'ivan-toggle-narrowing
    "x"        #'execute-extended-command))

(use-package dired-narrow
  :config
  (define-prefix-command 'ivan-dired-filter-map)
  (bind-keys
   :map ivan-dired-filter-map
   ("n"   . dired-narrow)
   ("SPC" . dired-narrow)
   ("r"   . dired-narrow-regexp)
   ("f"   . dired-narrow-fuzzy)
   ("/"   . revert-buffer)
   :map dired-narrow-map
   ("<escape>" . minibuffer-keyboard-quit))
  (bind-map-for-mode-inherit ivan-dired-leader-map ivan-leader-map
    :major-modes (dired-mode)
    :bindings
    ("/" ivan-dired-filter-map)))

(use-package nlinum-relative
  :commands nlinum-relative-toggle
  :init
  (setq nlinum-relative-redisplay-delay 0.0)
  (setq-default nlinum-relative-current-symbol "0")
  (defvar-local ivan-line-numbers-p nil
    "Whether line-numbers should be displayed.")
  (defvar-local ivan-relative-line-numbers-p nil
    "Whether relative line-numbers should be displayed.")
  (use-package nlinum
    :init
    (add-hook 'prog-mode-hook #'ivan-init-line-numbers)
    (defun ivan-init-line-numbers ()
      (unless (string-match-p "^[ *]" (buffer-name))
        (ivan-toggle-line-numbers)))
    :config (setq nlinum-format "%d "))
  (defun ivan-toggle-line-numbers ()
    (interactive)
    (setq-local ivan-line-numbers-p (not ivan-line-numbers-p))
    (ivan--update-relative-line-numbers-style)
    (unless ivan-relative-line-numbers-p (ivan--update-line-numbers-display))
    )
  (defun ivan-toggle-relative-line-numbers ()
    (interactive)
    (setq-local ivan-relative-line-numbers-p (not ivan-relative-line-numbers-p))
    (ivan--update-relative-line-numbers-display)
    )
  (defun ivan--update-relative-line-numbers-style ()
    (setq nlinum-relative-current-symbol (if ivan-line-numbers-p "" "0"))
    )
  (defun ivan--update-line-numbers-display ()
    (nlinum-mode (if ivan-line-numbers-p +1 0))
  )
  (defun ivan--update-relative-line-numbers-display ()
    (if ivan-relative-line-numbers-p
        (ivan--relative-line-numbers-on)
      (ivan--relative-line-numbers-off))
    )
  (defun ivan--relative-line-numbers-on ()
    (nlinum-mode +1)
    (nlinum-relative-on)
    )
  (defun ivan--relative-line-numbers-off ()
    (nlinum-relative-off)
    (ivan--update-line-numbers-display)
    )
  (bind-map-set-keys ivan-leader-map
    "n l" #'ivan-toggle-line-numbers
    "n r" #'ivan-toggle-relative-line-numbers
    )
  )

(use-package indent-guide
  :commands indent-guide-mode
  :init
  (bind-map-set-keys ivan-leader-map
    "C-t i" #'indent-guide-mode
    )
  )

(use-package manage-minor-mode
  :commands manage-minor-mode
  :init
  (bind-map-set-keys ivan-leader-map
    "M" #'manage-minor-mode
    )
  :config
  (evil-define-key 'normal manage-minor-mode-map (kbd "q") #'quit-window))

(use-package evil-commentary
  :diminish evil-commentary-mode
  :init
  (progn
    (evil-define-key 'normal evil-commentary-mode-map "g" nil)
    (bind-map-set-keys ivan-leader-map ";" #'evil-commentary)
    (evil-commentary-mode +1)))

(use-package goto-chg
  :commands (goto-last-change
             goto-last-change-reverse))

(use-package ffap
  :commands ffap-other-window)

(use-package origami
  :commands ivan-toggle-origami-mode
  :init
  (defvar ivan-evil-fold-map
    (copy-keymap (lookup-key evil-normal-state-map "z"))
    "Reference to evil fold keybindings.")
  (bind-map-set-keys ivan-leader-map "z" #'ivan-toggle-origami-mode)
  :config
  (setq origami-fold-replacement "···")
  (defun ivan-toggle-origami-mode ()
    (interactive)
    (if origami-mode
        (let ((map (copy-keymap ivan-evil-fold-map)))
          (origami-mode 0)
          (bind-key "z" map evil-normal-state-map)
          (message "evil folds"))
      (origami-mode +1)
      (message "origami folds")
      (ivan-setup-origami-keybindings))
    )
  (defun ivan-setup-origami-keybindings ()
    (bind-keys
     :map evil-normal-state-map
     ("z o"   . origami-open-node)
     ("z O"   . origami-open-node-recursively)
     ("z C-o" . origami-show-node)
     ("z c"   . origami-close-node)
     ("z C"   . origami-close-node-recursively)
     ("z a"   . origami-toggle-node)
     ("z f"   . origami-forward-toggle-node)
     ("z A"   . origami-recursively-toggle-node)
     ("z n"   . origami-open-all-nodes)
     ("z m"   . origami-close-all-nodes)
     ("z i"   . origami-toggle-all-nodes)
     ("z v"   . origami-show-only-node)
     ("z ["   . origami-previous-fold)
     ("z }"   . origami-next-fold)
     ("z ]"   . origami-forward-fold)
     ("z j"   . origami-forward-fold-same-level)
     ("z k"   . origami-backward-fold-same-level)
     ("z u"   . origami-undo)
     ("z U"   . origami-redo)
     ("z N"   . origami-reset)
     )
    )
  )

(use-package hideshow
  :commands (hs-minor-mode
             hs-toggle-hiding
             hs-already-hidden-p)
  :config (setq hs-isearch-open t)
  :init
  (defun doom*load-hs-minor-mode ()
    (hs-minor-mode 1)
    (advice-remove 'evil-toggle-fold 'doom-load-hs-minor-mode))
  (advice-add 'evil-toggle-fold :before 'doom*load-hs-minor-mode)
  (defface hs-face '((t (:background "#ff8")))
    "Face to hightlight the ... area of hidden regions"
    :group 'hideshow)
  (defface hs-fringe-face '((t (:foreground "#B3B3B3")))
    "Face used to highlight the fringe on folded regions"
    :group 'hideshow)
  (define-fringe-bitmap 'fold-marker [128 192 224 240 224 192 128] nil nil 'center)
  (setq hs-set-up-overlay
        (lambda (ov)
          (when (eq 'code (overlay-get ov 'hs))
            (let* ((marker-string "*")
                   (display-string "···")
                   (len (length display-string)))
              (put-text-property 0 1 'display
                                 (list 'left-fringe 'fold-marker 'hs-fringe-face)
                                 marker-string)
              (put-text-property 0 len 'face 'hs-face display-string)
              (overlay-put ov 'before-string marker-string)
              (overlay-put ov 'display display-string))))))

(use-package git-link
  :config
  (progn
    (defun ivan-open-git-link-in-browser (remote start end)
      (interactive (let* ((remote (if current-prefix-arg
                                      (git-link--read-remote)
                                    (git-link--remote)))
                          (region (git-link--get-region)))
                     (list remote (car region) (cadr region))))
      (let ((git-link-open-in-browser t))
        (git-link remote start end)))
    (bind-map-set-keys ivan-leader-map
      "v l" #'git-link
      "v L" #'ivan-open-git-link-in-browser)
    (defun ivan-git-link-github-handy (hostname dirname filename branch commit start end)
      (format "https://github.com/%s/blob/%s/%s#%s"
              dirname
              (or branch commit)
              filename
              (if end
                  (format "L%s-L%s" start end)
                (format "L%s" start))))
    (defun ivan-git-link-commit-github-handy (hostname dirname commit)
      (format "https://github.com/%s/commit/%s"
              dirname
              commit))
    (add-to-list 'git-link-remote-alist        '("github-handy" ivan-git-link-github-handy))
    (add-to-list 'git-link-commit-remote-alist '("github-handy" ivan-git-link-commit-github-handy))
    )
  )

(use-package hydra
  :bind ("M-S-<return>" . hydra-focus/body)
  :config
  (progn
    (defvar hydra-scroll/lock 'always)
    (defvar hydra-scroll/other-window nil)
    (defface hydra-face-title
      '((t (:slant italic)))
      "Face for title string within a hydra hint"
      :group 'hydra)
    (defhydra hydra-scroll (:hint nil
                            :foreign-keys run
                            :pre (setq hydra-lv nil)
                            :post (progn
                                    (setq-local hydra-scroll/lock 'always)
                                    (setq hydra-scroll/other-window nil))
                            :after-exit (setq hydra-lv t))
      (format "%s (_SPC_/_S-SPC_) page  (_j_/_k_) line"
              "%s(hydra-scroll/window-hint)")
      ("SPC"           hydra-scroll/pgdown)
      ("S-SPC"         hydra-scroll/pgup)
      ("j"             hydra-scroll/next-line)
      ("C-j"           hydra-scroll/next-line-with-lock)
      ("<down>"        evil-next-line)
      ("k"             hydra-scroll/previous-line)
      ("C-k"           hydra-scroll/previous-line-with-lock)
      ("<up>"          evil-previous-line)
      ("."             hydra-scroll/toggle-lock)
      (","             hydra-scroll/toggle-other-window)
      ("<escape>"      nil)
      ("q"             nil)
      )
    (bind-key "C-S-SPC" #'hydra-scroll/body)
    (defun hydra-scroll/locked ()
      (eql 'always hydra-scroll/lock))
    (defun hydra-scroll/lock-hint ()
      (if (hydra-scroll/locked) 'unlock 'lock))
    (defun hydra-scroll/window-hint ()
      (let ((other (if hydra-scroll/other-window "-other" ""))
            (unlocked (if (hydra-scroll/locked) "" "-unlocked")))
        (propertize
         (format " scroll%s%s " other unlocked) 'face 'hydra-face-title)))
    (defun hydra-scroll/toggle-lock ()
      (interactive)
      (setq-local hydra-scroll/lock
                  (if (hydra-scroll/locked) t 'always)))
    (defun hydra-scroll/toggle-other-window ()
      (interactive)
      (setq-local hydra-scroll/other-window
                  (null hydra-scroll/other-window)))
    (defun hydra-scroll/pgdown ()
      (interactive)
      (if hydra-scroll/other-window (scroll-other-window) (Info-scroll-up)))
    (defun hydra-scroll/pgup ()
      (interactive)
      (if hydra-scroll/other-window (scroll-other-window-down) (Info-scroll-down)))
    (defun hydra-scroll/next-line ()
      (interactive)
      (let ((scroll-preserve-screen-position hydra-scroll/lock))
        (if hydra-scroll/other-window (scroll-other-window 1) (scroll-up-line))))
    (defun hydra-scroll/next-line-with-lock ()
      (interactive)
      (let ((hydra-scroll/lock 'always))
        (hydra-scroll/next-line)))
    (defun hydra-scroll/previous-line ()
      (interactive)
      (let ((scroll-preserve-screen-position hydra-scroll/lock))
        (if hydra-scroll/other-window (scroll-other-window-down 1) (scroll-down-line))))
    (defun hydra-scroll/previous-line-with-lock ()
      (interactive)
      (let ((hydra-scroll/lock 'always))
        (hydra-scroll/previous-line)))
    (defhydra hydra-windsize (:hint nil
                              :pre (setq hydra-lv nil)
                              :after-exit (setq hydra-lv t))
      (format (propertize "windsize" 'face 'hydra-face-title))
      ("h"        windsize-left)
      ("<left>"   windsize-left)
      ("j"        windsize-down)
      ("<down>"   windsize-down)
      ("k"        windsize-up)
      ("<up>"     windsize-up)
      ("l"        windsize-right)
      ("<right>"  windsize-right)
      ("C-w h"    evil-window-left)
      ("C-w j"    evil-window-down)
      ("C-w k"    evil-window-up)
      ("C-w l"    evil-window-right)
      ("C-w C-h"  evil-window-left)
      ("C-w C-j"  evil-window-down)
      ("C-w C-k"  evil-window-up)
      ("C-w C-l"  evil-window-right)
      (","        evil-window-next)
      ("0"        delete-window :color blue)
      ("m"        shrink-window-if-larger-than-buffer)
      ("<escape>" nil)
      ("q"        nil))
    (bind-map-set-keys ivan-leader-map
      "C-w" #'hydra-windsize/body
      )
    (bind-keys
     :map evil-normal-state-map
     ("C-w ."   . hydra-windsize/body)
     ("C-w C-." . hydra-windsize/body)
     :map evil-motion-state-map
     ("C-w ."   . hydra-windsize/body)
     ("C-w C-." . hydra-windsize/body)
     )

    ;; TODO: merge zoom/narrow behavior into this hydra and give it a better keybinding
    (defhydra hydra-focus ()
      "focus"
      ("]"        ivan-increase-padding "increase")
      ("["        ivan-reduce-padding   "reduce")
      ("RET"      ivan-toggle-padding   "toggle")
      ("q"        nil "quit" :color blue)
      ("<escape>" nil "quit" :color blue)))

  (defhydra hydra-smartparens (:foreign-keys run)
    "sexps"
    ("q" nil)
    ("<escape>" nil)
    ("C-0" sp-forward-slurp-sexp  "forward-slurp")
    ("C-9" sp-forward-barf-sexp   "forward-barf")
    ("M-9" sp-backward-slurp-sexp "backward-slurp")
    ("M-0" sp-backward-barf-sexp  "backward-barf")
    ("t"   sp-transpose-sexp      "transpose")
    )

  (defhydra hydra-imenu-list (:hint nil
                              :pre (setq hydra-lv nil)
                              :after-exit (setq hydra-lv t)
                              :foreign-keys run)
    ("q" nil)
    ("<escape>" nil)
    ("SPC" #'imenu-list-display-entry))

  (defhydra hydra-dir-navigate (:hint nil
                                :pre (setq hydra-lv nil)
                                :after-exit (setq hydra-lv t))
    (format (propertize "dir-navigate" 'face 'hydra-face-title))
    ("u"        dired-jump)
    ("l"        ivan-dired-find-alternate-file)
    ("m"        ivan-dired-find-alternate-file)
    ("RET"      ivan-dired-find-alternate-file)
    ("<return>" ivan-dired-find-alternate-file)
    ("j"        dired-next-line)
    ("n"        dired-next-line)
    ("k"        dired-previous-line)
    ("p"        dired-previous-line)
    ("M-j"      evil-scroll-line-down)
    ("M-k"      evil-scroll-line-up)
    ("H"        evil-window-top)
    ("M"        evil-window-middle)
    ("L"        evil-window-bottom)
    ("("        dired-hide-details-mode)
    )

  (defun ivan-dired-find-alternate-file ()
    (interactive)
    (dired-find-alternate-file)
    (unless (eq major-mode 'dired-mode) (setq hydra-deactivate t)))
  )

(use-package which-key
  :init (which-key-mode +1)
  :diminish which-key-mode
  :config
  (progn
    (which-key-declare-prefixes
      "SPC b"     "branches/buffers/bookmarks"
      "SPC b m"   "bookmarks"
      "SPC f"     "files"
      "SPC g"     "git"
      "SPC m"     "mode"
      "SPC m e"   "eval"
      "SPC s"     "search"
      "SPC C-SPC" "scroll"
      )
    )
  )

(use-package drag-stuff
  :demand
  :diminish drag-stuff-mode
  :config
  (progn
    (with-eval-after-load 'evil
      (evil-define-key 'normal drag-stuff-mode-map
        (kbd "C-<up>") #'drag-stuff-up
        (kbd "C-<down>") #'drag-stuff-down
        (kbd "C-<right>") #'drag-stuff-right
        (kbd "C-<left>") #'drag-stuff-left)
      (evil-define-key 'visual drag-stuff-mode-map
        (kbd "C-<up>") #'drag-stuff-up
        (kbd "C-<down>") #'drag-stuff-down)
      )
    (drag-stuff-global-mode +1))
  )

(use-package company
  :commands (company-mode
             global-company-mode)
  :init
  (defun check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))
  (defun tab-indent-or-complete ()
    (interactive)
    (if (minibufferp)
        (minibuffer-complete)
      (if (and company-mode (check-expansion))
          (company-complete-common)
        (indent-for-tab-command))))
  (define-key evil-insert-state-map [tab] #'tab-indent-or-complete)
  :bind
  (:map company-active-map
        ("C-n"       . company-select-next)
        ("C-p"       . company-select-previous)
        ("<tab>"     . company-complete-common-or-cycle)
        ("TAB"       . company-complete-common-or-cycle)
        ("<backtab>" . company-select-previous)
        ("S-TAB"     . company-select-previous)
        ("C-e"       . company-abort)
        ("C-w"       . nil)
        ("C-SPC"     . company-show-location)
        )
  :config
  (setq
   company-backends '((company-dabbrev-code
                       company-gtags
                       company-etags
                       company-keywords)
                      company-capf
                      company-files
                      company-dabbrev)
   company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                       company-preview-frontend)
   company-auto-complete t
   company-auto-complete-chars '(?\s ?\( ?\) ?\, ?\.)
   company-idle-delay nil
   company-dabbrev-ignore-case nil
   company-dabbrev-downcase nil
   company-require-match 'never
   company-global-modes '(not eshell-mode
                              comint-mode
                              erc-mode
                              message-mode
                              help-mode)
   )
  (push 'company-sort-by-occurrence company-transformers)

  (with-eval-after-load 'evil
    (defun doom/company-evil-complete-next (&optional arg)
      "dabbrev wrapper for `evil-complete-next'"
      (call-interactively 'company-dabbrev)
      (if (eq company-candidates-length 1)
          (company-complete)))

    (defun doom/company-evil-complete-previous (&optional arg)
      "dabbrev wrapper for `evil-complete-previous'"
      (let ((company-selection-wrap-around t))
        (call-interactively 'company-dabbrev)
        (if (eq company-candidates-length 1)
            (company-complete)
          (call-interactively 'company-select-previous))))

    (setq evil-complete-next-func     'doom/company-evil-complete-next
          evil-complete-previous-func 'doom/company-evil-complete-previous))

  (global-company-mode +1))

(use-package quickrun
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region)
  :init (add-hook 'quickrun/mode-hook 'linum-mode)
  (bind-map ivan-quickrun-normal-leader-map
    :evil-keys ("SPC")
    :evil-states (normal motion)
    :bindings ("r" #'quickrun))
  (bind-map ivan-quickrun-visual-leader-map
    :evil-keys ("SPC")
    :evil-states (visual)
    :bindings ("r" #'quickrun-region
               "R" #'quickrun-replace-region))
  :config
  (setq quickrun-focus-p nil)
  (defun doom|quickrun-after-run ()
    "Ensures window is scrolled to BOF"
    (with-selected-window (get-buffer-window quickrun/buffer-name)
      (goto-char (point-min))))
  (add-hook 'quickrun-after-run-hook 'doom|quickrun-after-run)
  (with-eval-after-load 'evil
    (evil-define-key 'normal quickrun/mode-map
      (kbd "q") #'quit-window)))

(use-package repl-toggle
  :commands (rtog/toggle-repl)
  :preface (defvar rtog/mode-repl-alist nil)
  :init
  (defmacro def-repl! (mode command)
    "Define a REPL for a mode."
    `(push '(,mode . ,command) rtog/mode-repl-alist)))

(use-package recentf
  :init
  (defun ivan-recentf ()
    (interactive)
    (require 'recentf)
    (recentf-mode)
    (ivy-read "Recentf: " recentf-list
              :action
              (lambda (f)
                (with-ivy-window
                 (find-file f)))
              :caller 'ivy-recentf))
  :config
  (setq
   recentf-save-file (ivan-cache-file "recentf")
   recentf-exclude (append recentf-exclude
                           '("/tmp/" "/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
                             "emacs\\.d/private/cache/.+" "emacs\\.d/workgroups/.+$"
                             "wg-default" "/company-statistics-cache.el$"
                             "^/var/folders/.+$" "^/tmp/.+"))
   recentf-max-menu-items 0
   recentf-max-saved-items 250
   recentf-auto-cleanup 600
   recentf-filename-handlers '(abbreviate-file-name)))

(use-package ivy
  :commands (ivy-mode ivy-read ivy-completing-read ivy-recentf)
  :diminish 'ivy-mode
  :init
  (setq projectile-completion-system   'ivy
        magit-completing-read-function 'ivy-completing-read)
  :config
  (bind-keys
   :map ivy-minibuffer-map
   ("<escape>" . minibuffer-keyboard-quit)
   ("C-u"      . backward-kill-sentence)
   ("M-v"      . yank)
   ("C-n"      . ivy-next-line-or-history)
   ("C-p"      . ivy-previous-line-or-history)
   ("C-."      . ivy-next-history-element))
  (setq
   ivy-count-format ""
   ivy-format-function 'ivy-format-function-line
   ))

(use-package ivy-hydra :commands 'hydra-ivy/body)

(use-package swiper
  :commands swiper
  :init
  (with-eval-after-load 'bind-map
    (bind-map-set-keys ivan-leader-map "C-SPC" #'swiper))
  :config
  (autoload 'string-trim-right "subr-x"))

(use-package counsel
  :commands counsel-ag
  :init
  (defun ivan-counsel-ag-project ()
    (interactive)
    (counsel-ag nil (projectile-project-root)))
  (bind-map-set-keys ivan-leader-map
    "C-r" #'ivan-counsel-ag-project))

(use-package flx
  :defer t)

(use-package ag
  :commands
  (
   ag
   ag-files
   ag-regexp
   ag-project
   ag-project-files
   ag-project-regexp
   ag-dired
   ag-dired-regexp
   ag-project-dired
   ag-project-dired-regexp
   ag/dwim-at-point
   )
  :config
  (progn
    (use-package dash)
    (use-package s)
    (setq
     ag-arguments (delete "--stats" ag-arguments)
     ag-highlight-search t)

    (define-key ag-mode-map
      [remap wgrep-change-to-wgrep-mode] #'ivan-init-wgrep-mode)

    (defun ivan-filter-ag-whitespace ()
      (ivan-filter-whitespace ag/file-column-pattern))
    (advice-add 'ag-filter :after #'ivan-filter-ag-whitespace)

    (add-hook 'ag-search-finished-hook #'ivan-present-search-results)
    (add-hook 'ag-mode-hook #'hl-line-mode)
    (add-hook 'ag-mode-hook #'ivan-compilation-start-at-first-error))

  (defun ivan-ag-files-regexp (string file-type directory)
    "Search using ag in a given DIRECTORY for a given regexp,
limited to files that match FILE-TYPE."
    (interactive (list (ag/read-from-minibuffer "Search string")
                       (ag/read-file-type)
                       (read-directory-name "Directory: ")))
    (apply #'ag/search string directory :regexp t file-type)))


(use-package magnet
  :ensure nil
  :load-path "lisp/magnet"
  :commands (magnet--attract?) ; remove this once sibling behavior is pulled into magnet.el
  :bind (:map evil-motion-state-map ("C-SPC" . magnet-toggle))
  :init
  ;; TODO: pull this into magnet.el and provide a way for user to specify such sibling features
  (add-hook 'temp-buffer-show-hook
            (lambda ()
              (when (and (fboundp 'hidden-mode-line-mode)
                         (magnet--attract? (current-buffer)))
                (hidden-mode-line-mode))))
  :config
  (setq magnet-modes '(ag-mode
                       apropos-mode
                       help-mode
                       rspec-compilation-mode
                       ert-results-mode
                       ggtags-global-mode
                       quickrun/mode
                       rake-compilation-mode)
        magnet-names '("*Pp Eval Output*")))

(bind-map-for-mode-inherit ivan-compilation-leader-map ivan-leader-map
  :major-modes (compilation-mode)
  :bindings ("m f" #'next-error-follow-minor-mode))

(evil-define-key 'motion ert-results-mode-map (kbd "C-j") #'ert-results-next-test)
(evil-define-key 'motion ert-results-mode-map (kbd "C-k") #'ert-results-previous-test)
(evil-define-key 'motion compilation-mode-map (kbd "C-j") #'compilation-next-error)
(evil-define-key 'motion compilation-mode-map (kbd "C-k") #'compilation-previous-error)
(evil-define-key 'motion compilation-mode-map (kbd "M-n") #'make-frame)
(evil-define-key 'normal compilation-mode-map (kbd "M-n") #'make-frame)

(defun ivan-filter-whitespace (prefix-pattern)
  (save-excursion
    (forward-line 0)
    (let ((end (point)) beg)
      (goto-char compilation-filter-start)
      (forward-line 0)
      (setq beg (point))
      (when (< (point) end)
        (setq end (copy-marker end))
        ;; normalize whitespace between prefix-pattern and rest of line
        (while (re-search-forward prefix-pattern end 1)
          (and (re-search-forward "[[:blank:]]*" end 1)
               (replace-match " " t t)))))))

(defun ivan-present-search-results (&rest _args)
  (ignore-errors
    (select-window (get-buffer-window (compilation-find-buffer)))
    (recenter 0)))

(defun ivan-without-side-splits (orig-fun &rest args)
  (let ((split-width-threshold nil))
    (apply orig-fun args)))

(advice-add 'compilation-start :around #'ivan-without-side-splits)

(use-package ripgrep
  :commands
  (ripgrep-regexp
   projectile-ripgrep))

(use-package wgrep-ag
  :commands wgrep-ag-setup
  :init
  (add-hook 'ag-mode-hook 'wgrep-ag-setup)
  :config
  (progn
    (defun ivan-init-wgrep-mode ()
      (interactive)
      (advice-remove 'ag-filter #'ivan-filter-ag-whitespace)
      (add-hook 'ag-search-finished-hook #'ivan-enable-wgrep-mode)
      (recompile))
    (defun ivan-enable-wgrep-mode ()
      (remove-hook 'ag-search-finished-hook #'ivan-enable-wgrep-mode)
      (advice-add 'ag-filter :after #'ivan-filter-ag-whitespace)
      (wgrep-change-to-wgrep-mode +1))))

(use-package windsize
  :bind (
         ("C-S-<left>"  . windsize-left)
         ("C-S-<right>" . windsize-right)
         ("C-S-<up>"    . windsize-up)
         ("C-S-<down>"  . windsize-down)
         )
  :config
  (progn
    (setq windsize-rows 1 windsize-cols 2)
    (put 'windsize-down  'isearch-scroll t)
    (put 'windsize-up    'isearch-scroll t)
    (put 'windsize-left  'isearch-scroll t)
    (put 'windsize-right 'isearch-scroll t)))

(use-package org
  :bind
  (:map org-mode-map ("C-M-<return>" . ivan-org-midline-meta-return))
  :config
  (defun ivan-org-midline-meta-return ()
    (interactive)
    (end-of-line)
    (org-meta-return))
  (defun ivan-setup-org-mode ()
    (setq org-hide-leading-stars t
          line-spacing 0.15)
    (variable-pitch-mode +1))
  (add-hook 'org-mode-hook #'ivan-setup-org-mode)
  (add-hook 'org-mode-hook #'ivan-code-whitespace))

(use-package org-bullets
  :config
  (progn
    (setq org-bullets-bullet-list
          '("◉" "○" "•"))
    (add-hook 'org-mode-hook #'org-bullets-mode)))

(use-package graphviz-dot-mode
  :mode "\\.gv\\'")

(use-package coffee-mode
  :mode "\\.coffee\\'")

(use-package page-break-lines
  :diminish page-break-lines-mode
  :commands page-break-lines-mode
  :init
  (progn
    (add-hook 'help-mode-hook #'page-break-lines-mode)
    (add-hook 'Info-mode-hook #'page-break-lines-mode)))

(use-package tex-site
  :defines (latex-help-cmd-alist latext-help-file)
  :mode ("\\.text\\'" . TeX-latex-mode)
  :defer t
  :ensure auctex)

(use-package magit
  :commands (magit-status)
  :config
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
  (progn
    (setq
     magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
     magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
     )))

(use-package evil-magit
  :init
  (progn
    (setq
     evil-magit-use-y-for-yank nil
     evil-magit-want-horizontal-movement t
     )
    )
  :config
  (progn
    (bind-key "g s" 'magit-status evil-normal-state-map)
    (evil-define-key evil-magit-state magit-mode-map
      (kbd "n")   #'magit-section-forward
      (kbd "p")   #'magit-section-backward
      (kbd "P")   #'magit-push-popup
      (kbd "C-w") #'evil-window-map
      (kbd "y")   nil
      (kbd "yy")  #'evil-yank-line
      (kbd "yr")  #'magit-show-refs-popup
      (kbd "ys")  #'magit-copy-section-value
      (kbd "yb")  #'magit-copy-buffer-revision)
    (bind-map-for-mode-inherit ivan-with-editor-leader-map ivan-leader-map
      :minor-modes (with-editor-mode)
      :bindings
      ("w q RET" #'with-editor-finish)
      ("c q RET" #'with-editor-cancel))
    ))

(defun ivan-vc-refresh-state ()
  (interactive)
  (vc-refresh-state))

(use-package gitconfig-mode :mode ("/\\.?git/?config$" "/\\.gitmodules$"))
(use-package gitignore-mode :mode ("/\\.gitignore$" "/\\.git/info/exclude$" "/git/ignore$"))

(use-package git-messenger
  :commands git-messenger:popup-message
  :bind (:map git-messenger-map
              ("<escape>" . git-messenger:popup-close)
              ("q"        . git-messenger:popup-close))
  :init
  (defvar git-messenger-map (make-sparse-keymap))
  (bind-map-set-keys ivan-leader-map
    "v o" #'git-messenger:popup-message)
  :config
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t)
  )

(use-package flycheck
  :commands (flycheck-mode
             flycheck-buffer
             flycheck-has-current-errors-p
             flycheck-count-errors
             flycheck-list-errors)
  :init
  (setq
   flycheck-indication-mode 'right-fringe
   flycheck-check-syntax-automatically '(save mode-enabled)
   )
  :config
  (defun doom*flycheck-buffer ()
    (when (bound-and-true-p flycheck-mode)
      (flycheck-buffer)))
  (advice-add 'evil-force-normal-state :after 'doom*flycheck-buffer)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [0 0 0 0 0 4 12 28 60 124 252 124 60 28 12 4 0 0 0 0])
  (when (eq window-system 'mac)
    (use-package flycheck-pos-tip
      :init (flycheck-pos-tip-mode +1))))


(use-package flymake-ruby
  :diminish flymake-mode
  :init
  ;; (add-hook 'ruby-mode-hook 'flymake-ruby-load)
  )

(use-package rbenv
  :disabled t
  :init
  (setq
   rbenv-show-active-ruby-in-modeline nil
   rbenv-executable (substring (shell-command-to-string "which rbenv") 0 -1)
   )
  (global-rbenv-mode +1)
  (defun ivan-maybe-use-rbenv ()
    (when buffer-file-name
      (let ((inhibit-message t))
        (rbenv-use-corresponding))))
  (add-hook 'ruby-mode-hook #'ivan-maybe-use-rbenv))

(use-package haskell-mode
  :defer t)

(use-package projectile
  :commands (projectile-find-file
             projectile-project-root
             ivan-find-file-from-project-root)
  :bind (("M-O" . projectile-find-file)
         ("M-o" . ivan-find-file-from-project-root))
  :config
  (setq projectile-enable-caching t
        projectile-cache-file (ivan-cache-file "projectile.cache")
        projectile-known-projects-file (ivan-cache-file "projectile.projects"))

  (dolist (dir `(,ivan-cache-directory "assets" ".cask" ".sync"))
    (add-to-list 'projectile-globally-ignored-directories dir))
  (dolist (suf '(".elc" ".project"))
    (add-to-list 'projectile-globally-ignored-file-suffixes suf))

  (defun ivan-find-file-from-project-root ()
    (interactive)
    (let* ((projectile-require-project-root nil)
           (default-directory (projectile-project-root)))
      (call-interactively #'find-file)))

  (projectile-global-mode +1))

(use-package projectile-rails
  :diminish projectile-rails-mode
  :init
  (projectile-rails-global-mode +1)
  :config
  (setq rake-cache-file (ivan-cache-file "rake.cache")))

(use-package rspec-mode
  :pin melpa
  :diminish rspec-mode
  :commands (rspec-mode
             ivan-rspec-verify-dwim)
  :config
  (defun ivan-rspec-dwim (rspec-func)
    (interactive)
    (if (rspec-buffer-is-spec-p)
        (funcall rspec-func)
      (rspec-rerun)))

  (defun ivan-maybe-present-rspec-results (buffer _outcome)
    (when (eq 'rspec-compilation-mode major-mode)
      (ivan-present-rspec-results buffer)))

  (defun ivan-present-rspec-results (buffer)
    (let ((original-window (selected-window))
          (results-window  (get-buffer-window buffer 'visible)))
      (when results-window
        (select-window results-window)
        (next-line 3)
        (recenter-top-bottom 0)
        (select-window original-window))))

  (defun ivan-add-rspec-presenter ()
    (add-hook 'compilation-finish-functions #'ivan-maybe-present-rspec-results))
  (add-hook 'rspec-compilation-mode-hook #'ivan-add-rspec-presenter)
  (add-hook 'rspec-compilation-mode-hook #'ivan-wrap-lines)

  (with-eval-after-load 'evil
    (let ((gmap (lookup-key evil-motion-state-map "g")))
      (evil-define-key 'normal rspec-compilation-mode-map "g" gmap)
      (evil-define-key 'motion rspec-compilation-mode-map "g" gmap)))

  (bind-map-for-mode-inherit ivan-rspec-leader-map ivan-leader-map
    :major-modes (ruby-mode)
    :bindings
    ("t" #'rspec-toggle-spec-and-target)))

(use-package cask-mode
  :mode "\\`Cask\\'")

(use-package yaml-mode
  :mode "\\.yml\\'"
  :config
  (add-hook 'yaml-mode-hook #'ivan-truncate-lines))

(use-package haml-mode :mode "\\.haml$")

(use-package eshell
  :init
  (setq eshell-directory-name (ivan-cache-file "eshell/")))

(use-package exec-path-from-shell
  :defer t)

(use-package sh-script
  :mode ("/\\.?bash\\(/.*\\|rc\\|_profile\\|\\)$" . sh-mode)
  :init (add-hook 'sh-mode-hook (lambda () (setq mode-name "shell")))
  :config
  (setq sh-basic-offset 2
        sh-indentation  sh-basic-offset))

(use-package face-remap
  :diminish text-scale-mode
  :defer t)

(use-package re-builder
  :commands re-builder
  :init
  (defun doom|reb-cleanup ()
    (text-scale-set 2)
    (goto-char 3))
  (add-hook 'reb-mode-hook 'doom|reb-cleanup)
  :config
  (setq reb-re-syntax 'string)
  (with-eval-after-load 'evil
    (evil-define-key 'normal reb-mode-map
      [escape] 'reb-quit)))

(use-package rotate-text
  :ensure nil
  :load-path "lisp/rotate-text-20111203.2039"
  :commands (rotate-text rotate-text-backward)
  :config (push '("true" "false") rotate-text-words))

(use-package restclient
  :mode ("\\.\\(http\\|rest\\)$" . restclient-mode))

(add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name "Elisp")))

;; gui & terminal
(defun ivan-text-scale-reset ()
  "Reset the height of the default face in the current buffer to its default value.
Disables `text-scale-mode`."
  (interactive)
  (text-scale-set 0))

(defun ivan-kill-buffer-and-maybe-window ()
  (interactive)
  (if (one-window-p)
      (kill-this-buffer)
    (kill-buffer-and-window)))

(defun ivan-delete-window ()
  (interactive)
  (if (one-window-p)
      (delete-frame)
    (delete-window)))

(defun ivan-global-text-scale-increase ()
  (interactive)
  (set-face-attribute
   'default nil :height (round (* (face-attribute 'default :height) 1.2))))

(defun ivan-global-text-scale-decrease ()
  (interactive)
  (set-face-attribute
   'default nil :height (round (/ (face-attribute 'default :height) 1.2))))

(defun ivan-quit-no-confirm ()
  (interactive)
  (let ((confirm-kill-emacs nil))
    (save-buffers-kill-terminal)))

(defun configure-gui ()
  (bind-keys*
   ("M-c" . kill-ring-save)
   ("M-v" . yank))
  (bind-keys*
   :filter (not (minibufferp))
   ("M-<return>" . toggle-frame-fullscreen)
   ("M-q"        . save-buffers-kill-terminal)
   ("M-A"        . mark-whole-buffer)
   ("M-w"        . ivan-delete-window)
   ("M-W"        . delete-frame)
   ("M-m"        . iconify-frame)
   ("M-n"        . make-frame)
   ("M-s"        . save-buffer)
   ("M-u"        . ivan-toggle-transparency)
   ("M-="        . text-scale-increase)
   ("M--"        . text-scale-decrease)
   ("M-0"        . ivan-text-scale-reset)
   ("M-+"        . ivan-global-text-scale-increase)
   ("M-_"        . ivan-global-text-scale-decrease)
   ("M-`"        . ns-next-frame)
   ("M-~"        . ns-prev-frame)
   )
  (bind-keys
   ;; reconcile some overridden keybindings
   ("<escape> M-q" . fill-paragraph)
   ("<escape> M-o" . facemenu-keymap)
   ("<escape> M-c" . capitalize-word)
   ("<escape> M-u" . upcase-word)
   ("<escape> M-=" . count-words-region)
   )
  (bind-key "<escape> M-s" search-map)
  (setq exec-path
        (eval-when-compile
          (require 'exec-path-from-shell)
          (exec-path-from-shell-initialize)
          exec-path))
  (with-eval-after-load 'evil
    (fset 'evil-visual-update-x-selection 'ignore)))

(defun configure-terminal ()
  (xterm-mouse-mode +1)
  (setq
   mouse-sel-mode t
   mouse-wheel-follow-mouse 't
   )
  (bind-keys
   ("<mouse-4>" . scroll-down-line)
   ("<mouse-5>" . scroll-up-line)
   )
  (put 'scroll-down-line 'isearch-scroll t)
  (put 'scroll-up-line   'isearch-scroll t)

  (defun ivan-adjust-terminal-colors ()
    (unless (display-graphic-p (selected-frame))
      (set-face-background 'default "white" (selected-frame))
      (set-face-background 'hl-line "#EEEEEE" (selected-frame)))
    )
  (add-hook 'window-setup-hook 'ivan-adjust-terminal-colors)

  (defun my-change-window-divider ()
    (let ((display-table (or buffer-display-table standard-display-table)))
      (set-display-table-slot display-table 5 ?│)
      (set-window-display-table (selected-window) display-table)))
  (add-hook 'window-configuration-change-hook 'my-change-window-divider)
  )

(bind-keys
 ("S-<left>"  . left-word)
 ("S-<right>" . right-word))

(defconst IS-MAC (eq system-type 'darwin))

(use-package google-this
  :commands (google-this
             google-this-search
             google-this-word
             google-this-symbol
             google-this-region)
  :bind (("M-G" . google-this)))

(use-package dash-at-point
  :when IS-MAC
  :commands (dash-at-point
             dash-at-point-with-docset
             dash-at-point-run-search
             dash-at-point-guess-docset)
  :bind (("M-H" . dash-at-point)))

(use-package zeal-at-point
  :when (not IS-MAC)
  :commands (zeal-at-point
             zeal-at-point-set-docset)
  :bind (("M-H" . dash-at-point)))

(defun configure-mac-modifiers ()
  (setq mac-command-modifier 'meta))

(defun configure-mac-directory-program ()
  (if (file-exists-p "/usr/local/bin/gls")
      (setq insert-directory-program "/usr/local/bin/gls")
    (require 'ls-lisp)
    (setq ls-lisp-use-insert-directory-program nil))
  )

(when IS-MAC
  (configure-mac-modifiers)
  (configure-mac-directory-program)

  (defun doom-open-with (&optional app-name path)
    "Send PATH to APP-NAME on OSX."
    (interactive)
    (let* ((path (f-full (s-replace "'" "\\'"
                                    (or path (if (eq major-mode 'dired-mode)
                                                 (dired-get-file-for-visit)
                                               (buffer-file-name))))))
           (command (format "open %s"
                            (if app-name
                                (format "-a %s '%s'" (shell-quote-argument app-name) path)
                              (format "'%s'" path)))))
      (message "Running: %s" command)
      (shell-command command)))

  (defmacro def-open-with! (id &optional app dir)
    `(defun ,(intern (format "os-%s" id)) ()
       (interactive)
       (doom-open-with ,app ,dir)))

  (def-open-with! open-in-default-program)
  (def-open-with! open-in-browser "Google Chrome")
  (def-open-with! reveal "Finder" default-directory)

  (defun os-switch-to-term ()
    (interactive)
    (when (display-graphic-p)
      (do-applescript "tell application \"iTerm\" to activate")))

  (defun os-switch-to-term-and-cd ()
    (interactive)
    (when (display-graphic-p)
      (doom:send-to-tmux (format "cd %s" (shell-quote-argument default-directory))))
    (os-switch-to-term))
  )

(if (display-graphic-p)
    (configure-gui)
  (configure-terminal))


;; keybindings
;; Note that "function keys" (those with dedicated physical keys) are written in
;; angle brackets, whereas "control keys" are written in caps. Control keys can
;; be written in angle brackets as well, but it's not required.
;; So "<tab>" represents the physical Tab key, whereas "TAB" and "<TAB>" both
;; represent Ctrl-I. By default, the physical key translates to the same binding
;; as its control-key equivalent, but it can be explicitly bound differently.
(bind-keys
 ("M-/" . hippie-expand)
 ("C-/" . undo-tree-undo)
 ("C-?" . undo-tree-redo)
 :map isearch-mode-map
 ("C-n"  . isearch-repeat-forward)
 ("C-p"  . isearch-repeat-backward)
 ("<up>" . isearch-ring-retreat)
 :map rectangle-mark-mode-map
 ("s" . string-rectangle)
 ("o" . rectangle-exchange-point-and-mark)
 :map shell-mode-map
 ("C-d" . comint-delchar-or-eof-or-kill-buffer)
 )

(global-set-key (kbd "C-w") #'ivan-kill-region-or-backward-kill-word)

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

(defun ivan-kill-completions-buffer ()
  (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer))))

(add-hook 'minibuffer-exit-hook #'ivan-kill-completions-buffer)

(bind-key "<escape>" 'isearch-abort isearch-mode-map)

(mapc (lambda (keymap)
        (define-key keymap [escape]    #'minibuffer-keyboard-quit)
        (define-key keymap (kbd "C-p") #'previous-line-or-history-element)
        (define-key keymap (kbd "C-n") #'next-line-or-history-element))
      (list
       minibuffer-local-map
       minibuffer-local-ns-map
       minibuffer-local-completion-map
       minibuffer-local-must-match-map
       minibuffer-local-isearch-map
       ))

(defun ivan-isearch-exit ()
  "Run isearch-exit, and if in the minibuffer, submit the search result as input."
  (interactive)
  (isearch-exit)
  (when (minibuffer-window-active-p (selected-window))
    (let ((completion-fail-discreetly t))
      (minibuffer-complete-and-exit))))

;; hack through annoying minibuffer complaints
(defun ivan-command-error-function (data context caller)
  "Ignore the buffer-read-only signal; pass the rest to the default handler."
  (unless (eq 'buffer-read-only (car data))
    (command-error-default-function data context caller)))

(setq command-error-function #'ivan-command-error-function)

(defun ivan-minibuffer-C-w ()
  (interactive)
  (let ((fn (if (use-region-p) #'kill-region #'backward-kill-word)))
    (call-interactively fn)))

(add-hook 'minibuffer-setup-hook
          (lambda () (local-set-key (kbd "C-w") #'ivan-minibuffer-C-w)))


;; no truncation arrows
(set-display-table-slot standard-display-table 0 ?\ )

;; padding
(defvar ivan-padding-enabled nil)
(defvar ivan-padding-min 4)
(defvar ivan-padding-max 580)
(defvar ivan-padding-step 32)
(defvar ivan-padding-degree ivan-padding-min)

(add-to-list 'default-frame-alist `(left-fringe . ,ivan-padding-min))
(add-to-list 'default-frame-alist '(right-fringe . 1))

(define-fringe-bitmap 'left-bar [96 96 96 96 96 96 96 96 96 96 96 96])
(defface left-bar-face
  `((t (:foreground ,(face-foreground 'default))))
  "Face for `left-bar` bitmap.")
(set-fringe-bitmap-face 'left-bar 'left-bar-face)
(put 'overlay-arrow-position 'overlay-arrow-bitmap 'left-bar)

(defun ivan-increase-padding ()
  (interactive)
  (setq-local ivan-padding-degree
              (min ivan-padding-max (+ ivan-padding-degree ivan-padding-step)))
  (ivan--apply-padding-degree ivan-padding-degree)
  (setq-local ivan-padding-enabled t))

(defun ivan-reduce-padding ()
  (interactive)
  (setq-local ivan-padding-degree
              (max ivan-padding-min (- ivan-padding-degree ivan-padding-step)))
  (ivan--apply-padding-degree ivan-padding-degree)
  (setq-local ivan-padding-enabled t))

(defun ivan-toggle-padding ()
  (interactive)
  (ivan--apply-padding-degree
   (if ivan-padding-enabled ivan-padding-min ivan-padding-degree))
  (setq-local ivan-padding-enabled (not ivan-padding-enabled)))

(defun ivan--apply-padding-degree (n)
  (set-window-fringes nil n nil (when (<= n ivan-padding-min)
                                  fringes-outside-margins)))


;; etc.
(with-eval-after-load "isearch"
  (define-key isearch-mode-map (kbd "RET")        #'ivan-isearch-exit)
  (define-key isearch-mode-map (kbd "<return>")   #'ivan-isearch-exit)
  (define-key isearch-mode-map (kbd "S-<return>") #'isearch-exit))

(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(defun ivan-add-whitespace-below ()
  (interactive)
  (save-excursion
    (end-of-line)
    (newline)))

(defun ivan-add-whitespace-above ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (newline)))

(defun ivan-minibuffer-setup-hook ()
  (setq gc-cons-threshold 339430400))

(defun ivan-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'ivan-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'ivan-minibuffer-exit-hook)

(global-set-key [remap dabbrev-expand] #'hippie-expand)

(with-eval-after-load 'webjump
  (setq webjump-sites
        (append '(("stackoverflow" . "www.stackoverflow.com")
                  ("GitHub" . "https://github.com"))
                webjump-sample-sites)))

(use-package ruby-mode
  :config
  (add-hook 'ruby-mode-hook #'ivan-treat-underscore-as-word-char)
  (advice-add 'ruby-do-end-to-brace :after #'ivan-trim-whitespace-current-line)
  (advice-add 'ruby-brace-to-do-end :after #'ivan-trim-whitespace-next-line)

  (with-eval-after-load 'smartparens-ruby
    (advice-add 'sp-ruby-pre-pipe-handler :after #'ivan-trim-whitespace-current-line))

  (add-hook 'ruby-mode-hook #'(lambda ()
                                (setq ruby-insert-encoding-magic-comment nil
                                      ruby-align-chained-calls t))))

(use-package emr
  :commands emr-show-refactor-menu
  :config
  (emr-initialize)
  (define-key popup-menu-keymap [escape] 'keyboard-quit))

(use-package ruby-refactor
  :commands ruby-refactor-mode-launch
  :init
  (add-hook 'ruby-mode-hook #'ruby-refactor-mode-launch))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package vimrc-mode
  :mode (("\\`\\.?vimrc\\'" . vimrc-mode)
         ("\\.vim\\'"       . vimrc-mode)))

(use-package help-fns+
  :commands describe-keymap
  :config
  (define-key help-map "c"    'describe-key-briefly)
  (define-key help-map "\C-c" 'describe-command))

(use-package imenu-list
  :commands (imenu-list imenu-list-minor-mode)
  :config
  (setq imenu-list-mode-line-format nil
        imenu-list-position 'right
        imenu-list-size 40)

  (with-eval-after-load 'evil
    (evil-define-key 'motion imenu-list-major-mode-map
     [escape]      #'imenu-list-quit-window
     (kbd "q")     #'imenu-list-quit-window
     (kbd "RET")   #'imenu-list-goto-entry
     (kbd "C-SPC") #'hydra-imenu-list/imenu-list-display-entry
     [tab]         #'hs-toggle-hiding)

    (add-hook 'imenu-list-major-mode-hook
              (lambda ()
                (setq-local evil-motion-state-cursor
                            '((bar . 0)
                              (lambda ()
                                (evil-set-cursor-color (face-foreground 'mode-line-inactive))))))))

    (defun ivan-imenu-list-setup ()
      (setq line-spacing 2
            tab-width    1)
      (when (featurep 'hl-line)
        (set (make-local-variable 'hl-line-sticky-flag) t)
        (hl-line-mode +1)))

    (add-hook 'imenu-list-major-mode-hook #'ivan-imenu-list-setup))

(add-hook 'change-major-mode-hook #'ivan-treat-underscore-as-word-char)

(dolist (hook '(emacs-lisp-mode-hook
                clojure-mode-hook
                scheme-mode-hook
                lisp-mode-hook))
  (add-hook hook #'ivan-treat-hyphen-as-word-char))

(defun ivan-treat-underscore-as-word-char () (ivan-treat-as-word-char ?_))
(defun ivan-treat-hyphen-as-word-char     () (ivan-treat-as-word-char ?-))
(defun ivan-treat-as-word-char (char) (modify-syntax-entry char "w"))

(defun ivan-goto-match-beginning ()
  (when (and isearch-forward isearch-other-end
             (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))
(add-hook 'isearch-mode-end-hook #'ivan-goto-match-beginning)

;; more useful C-w (this should be adjusted to account for evil mode,
;; in particular insert-state, once I start using evil).
(defun ivan-kill-region-or-backward-kill-word (&optional arg region)
  "`kill-region' if the region is active, otherwise `backward-kill-word'"
  (interactive
   (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

;; diminish
(with-eval-after-load 'flyspell
  (diminish 'flyspell-mode))
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))

(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(with-eval-after-load 'ibuffer
  (require 'ibuffer-config))
(with-eval-after-load 'dired
  (require 'dired-config))
(with-eval-after-load 'view
  (setq view-inhibit-help-message t))

(setq
 calendar-latitude 40.7
 calendar-longitude -74.0
 calendar-location-name "New York, NY"
 )

;; Mitigate Bug#28350 (security) in Emacs 25.2 and earlier.
  (eval-after-load "enriched"
    '(defun enriched-decode-display-prop (start end &optional param)
       (list start end)))

(provide 'core)
