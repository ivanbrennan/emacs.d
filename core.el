;; blank slate
(setq
 inhibit-startup-screen t
 inhibit-startup-echo-area-message "ivan"
 initial-scratch-message nil
 )

;; If your init file is byte-compiled, use the following form instead:
;;  (eval \\='(setq inhibit-startup-echo-area-message \"YOUR-USER-NAME\"))

;; clean screen
(menu-bar-mode   0)
(tool-bar-mode   0)
(when (display-graphic-p)
  (scroll-bar-mode 0)
  (tooltip-mode    0)
  (setq frame-title-format "emacs : %b"))


;; coding
(prefer-coding-system 'utf-8)


;; parens
(show-paren-mode    +1)
(electric-pair-mode +1)

(setq
 blink-matching-paren 'jump
 blink-matching-delay 0.25
 underline-minimum-offset 5
 )

;; cursor
(blink-cursor-mode 0)

(setq-default
 cursor-type 'bar
 cursor-in-non-selected-windows nil
 )

(customize-set-variable
 'minibuffer-prompt-properties
 (quote (read-only t cursor-intangible t face minibuffer-prompt)))

(setq-default
 bidi-display-reordering nil
 fringe-indicator-alist  (let ((alist fringe-indicator-alist))
                           (setq alist (delq (assq 'continuation alist) alist))
                           (setq alist (delq (assq 'empty-line   alist) alist))
                           alist)
 fringes-outside-margins nil
 image-animate-loop      t
 jit-lock-stealth-nice   0.1
 jit-lock-stealth-time   0.2
 max-mini-window-height  0.3
 mode-line-default-help-echo nil
 show-help-function      nil
 show-paren-when-point-inside-paren t
 use-dialog-box          nil
 visible-cursor          nil
 resize-mini-windows     t
 confirm-kill-emacs      (lambda (_) (y-or-n-p "››› Quit?")))

;;; hidden mode line
(put 'hidden-mode-line-mode 'permanent-local t)
(put 'hidden-mode-line 'permanent-local t)

(defvar hidden-mode-line-format nil
  "Format to use when `hidden-mode-line-mode' replaces the modeline")

(defvar-local hidden-mode-line nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  (if hidden-mode-line-mode
      (setq hidden-mode-line mode-line-format
            mode-line-format hidden-mode-line-format)
    (setq mode-line-format hidden-mode-line
          hidden-mode-line hidden-mode-line-format)))

(with-current-buffer "*Messages*" (hidden-mode-line-mode +1))
(dolist (hook '(help-mode-hook
                apropos-mode-hook
                ibuffer-mode-hook
                compilation-mode-hook
                messages-buffer-mode-hook
                completion-list-mode-hook))
  (add-hook hook #'hidden-mode-line-mode))

(add-hook 'compilation-mode-hook (lambda () (setq-local scroll-margin 0)))

;;; A subtle bell: flash the mode-line
;; TODO More flexible colors
(defvar doom--modeline-bg nil)
(defface doom-modeline-flash '((t (:inherit mode-line :background "#54252C")))
  "Face used for the mode-line ring-bell-function.")

(defsubst ivan/modeline-flash ()
  (unless doom--modeline-bg
    (setq doom--modeline-bg (face-background 'mode-line)))
  (set-face-background 'mode-line
                       (face-background 'doom-modeline-flash))
  (run-with-timer
   0.1 nil
   (lambda ()
     (when doom--modeline-bg
       (set-face-background 'mode-line doom--modeline-bg)
       (setq doom--modeline-bg nil)))))

(setq ring-bell-function #'ivan/modeline-flash)

(global-eldoc-mode 0)
(add-hook 'emacs-lisp-mode-hook  #'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

(defun add-todo-and-note-keywords ()
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\(?:(.*)\\)?:?\\)\\>"  1 'warning prepend)
         ("\\<\\(NOTE\\(?:(.*)\\)?:?\\)\\>"  1 'success prepend))))

(dolist (hook '(prog-mode-hook
                emacs-lisp-mode-hook
                css-mode-hook))
  (add-hook hook #'add-todo-and-note-keywords))

(setq
 window-divider-default-places       t
 window-divider-default-bottom-width 1
 window-divider-default-right-width  1)
(window-divider-mode +1)

;; persistence
(defconst ivan/emacs-dir
  (expand-file-name user-emacs-directory)
  "Location of emacs.d directory.")

(defconst ivan/cache-dir
  (expand-file-name ".cache" ivan/emacs-dir)
  "Storage area for persistent files.")

(defconst ivan/config-dir
  (expand-file-name "config" ivan/emacs-dir)
  "Directory for feature configuration files.")

(defconst ivan/packages-dir
  (expand-file-name "packages" ivan/emacs-dir)
  "Directory for packages.")

(defsubst ivan/emacs-file (f) (expand-file-name f ivan/emacs-dir))
(defsubst ivan/cache-file (f) (expand-file-name f ivan/cache-dir))

(make-directory ivan/cache-dir 'mkdir_p)
(make-directory (ivan/cache-file "auto-save") 'mkdir_p)

(setq custom-file (ivan/emacs-file "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(setq
 auto-save-file-name-transforms `((".*" ,(ivan/cache-file "auto-save/") 'uniquify))
 auto-save-list-file-prefix      (ivan/cache-file "auto-save-list/.saves-")
 backup-by-copying               t
 backup-directory-alist         `(("." . ,(ivan/cache-file "backups/")))
 eshell-directory-name           (ivan/cache-file "eshell/")
 ido-save-directory-list-file    (ivan/cache-file "ido.last")
 savehist-file                   (ivan/cache-file "savehist")
 tramp-persistency-file-name     (ivan/cache-file "tramp")
 )

(savehist-mode +1)


;; theme
(setq custom-theme-directory (ivan/emacs-file "themes/"))
(make-directory custom-theme-directory 'mkdir_p)

(defvar ivan/themes '(elixir doom-one))
(defvar ivan/themes-index 0)

(defvar ivan/rotated-theme-hook nil
  "Hook called after the theme has been rotated")

(defun ivan/rotate-theme ()
  (interactive)
  (setq ivan/themes-index (% (1+ ivan/themes-index) (length ivan/themes)))
  (ivan/try-load-indexed-theme)
  (run-hooks 'ivan/rotated-theme-hook))

(defun ivan/try-load-indexed-theme ()
  (ivan/try-load-theme (nth ivan/themes-index
                            ivan/themes)))

(defun ivan/try-load-theme (theme)
  (let ((theme  (nth ivan/themes-index ivan/themes))
        (backup (ivan/disable-themes)))
    (unless (ignore-errors (ivan/load-theme theme))
      (ivan/restore-themes backup))))

(defun ivan/load-theme (theme)
  (load-theme theme 'no-confirm))

(defun ivan/disable-themes ()
  (mapc #'disable-theme custom-enabled-themes))

(defun ivan/restore-themes (backup)
  (mapc #'disable-theme custom-enabled-themes)
  (mapc #'ivan/load-theme (reverse backup)))

(ivan/try-load-indexed-theme)


;; variable-pitch-mode
(add-hook 'help-mode-hook #'variable-pitch-mode)
(add-hook 'Info-mode-hook #'variable-pitch-mode)


;; transparency
(defun ivan/toggle-transparency ()
  (interactive)
  (let* ((alpha     (frame-parameter nil 'alpha))
         (opaque    (or (null alpha) (eql 100 alpha)))
         (new-value (if opaque '(97 . 85) 100)))
    (set-frame-parameter nil 'alpha new-value)))


;; line-wrapping
(defun ivan/truncate-lines ()
  (setq truncate-lines t))
(defun ivan/wrap-lines ()
  (setq truncate-lines nil))

(dolist (hook '(prog-mode-hook
                compilation-mode-hook
                occur-mode-hook))
  (add-hook hook #'ivan/truncate-lines))

(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'help-mode-hook #'visual-line-mode)

(with-current-buffer "*Messages*"
  (visual-line-mode +1))


;; splits, frames, windows
(setq split-width-threshold 130)
(setq
 display-buffer-alist
 `(
   ;; Put search results in bottom side-window of the current frame.
   (,(rx bos
         (or
          (and "*ag " (1+ not-newline) "*")
          "*ggtags-global*"
          "*Help*"
          "*Apropos*"
          "*rake-compilation*"
          )
         eos)
    (display-buffer-reuse-window
     display-buffer-in-side-window)
    (reusable-frames)
    (side . bottom)
    )
   ;; Put test results in reusable window/frame if one is visible,
   ;; otherwise put them in bottom side-window.
   (,(rx bos
         (or
          "*rspec-compilation*"
          "*ert*"
          )
         eos)
    (display-buffer-reuse-window
     display-buffer-in-side-window)
    (reusable-frames . visible)
    (inhibit-switch-frame . t)
    (side . bottom)
    )
   ))

;; scroll
(setq
 scroll-step    1
 scroll-margin  1
 hscroll-step   1
 hscroll-margin 2
 scroll-conservatively 200
 mouse-wheel-scroll-amount '(0.01 ((shift) . 1))
 isearch-allow-scroll t
 )

(defun ivan/scroll-right ()
  (interactive)
  (scroll-right 2))

(defun ivan/scroll-left ()
  (interactive)
  (scroll-left 2))

(defvar ivan/hscroll-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [C-wheel-right] #'ivan/scroll-left)
    (define-key map [S-wheel-right] #'ivan/scroll-left)
    (define-key map [C-wheel-left]  #'ivan/scroll-right)
    (define-key map [S-wheel-left]  #'ivan/scroll-right)
    map)
  "ivan/hscroll-minor-mode keymap.")

(define-minor-mode ivan/hscroll-minor-mode
  "A minor mode so my horizontal scroll bindings take precedence."
  :init-value t)

(ivan/hscroll-minor-mode +1)

(put 'mac-mwheel-scroll 'isearch-scroll t)
(put 'ivan/scroll-right 'isearch-scroll t)
(put 'ivan/scroll-left  'isearch-scroll t)
(put 'hl-line-mode      'isearch-scroll t)


;; whitespace
(setq
 whitespace-line-column 90
 whitespace-style '(
                    empty
                    face
                    indentation
                    lines-tail
                    space-after-tab
                    space-before-tab
                    trailing
                    )
 )

(defun ivan/code-whitespace ()
  (setq
   indent-tabs-mode         nil
   indicate-empty-lines     t
   show-trailing-whitespace t
   ))

(add-hook 'prog-mode-hook #'ivan/code-whitespace)
(add-hook 'org-mode-hook  #'ivan/code-whitespace)


;; sensibility
(setq
 ad-redefinition-action              'accept
 apropos-do-all                      t
 bookmark-bmenu-toggle-filenames     nil
 bookmark-default-file               (ivan/cache-file "bookmarks")
 comint-prompt-read-only             t
 compilation-always-kill             t
 compilation-ask-about-save          nil
 compilation-message-face            nil  ; don't underline compilation links
 compilation-save-buffers-predicate  (lambda () (eq (window-buffer) (current-buffer)))
 completions-format                  'vertical
 delete-by-moving-to-trash           t
 echo-keystrokes                     0.5
 ediff-split-window-function         #'split-window-horizontally
 ediff-window-setup-function         'ediff-setup-windows-plain
 enable-recursive-minibuffers        t
 eval-expression-print-length        192
 find-file-visit-truename            t
 help-window-select                  t
 hi-lock-auto-select-face            t
 history-delete-duplicates           t
 history-length                      500
 idle-update-delay                   2
 ispell-program-name                 "/usr/local/bin/aspell"
 load-prefer-newer                   t
 minibuffer-eldef-shorten-default    t
 query-replace-skip-read-only        t
 read-buffer-completion-ignore-case  t
 require-final-newline               t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position     t
 sentence-end-double-space           nil
 set-mark-command-repeat-pop         t
 split-window-keep-point             nil
 tab-always-indent                   'complete
 uniquify-buffer-name-style          'forward
 vc-follow-symlinks                  t
 x-select-request-type               '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
 )

(fset 'yes-or-no-p 'y-or-n-p)

(defalias 'list-buffers 'ibuffer)
(minibuffer-electric-default-mode +1)
(delete-selection-mode +1)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'tex-mode-hook #'(lambda () (setq ispell-parser 'tex)))

(defun ivan/create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory ‘%s’ does not exist! Create it?" parent-directory)))
      (make-directory parent-directory 'mkdir_p))))

(add-to-list 'find-file-not-found-functions
             'ivan/create-non-existent-directory)


;; env
(setenv "PAGER" "/usr/bin/env cat")


;; load-path
(defvar initial-load-path load-path
  "Initial `load-path', used as a base so reloads are idempotent.")

(setq load-path (append (list ivan/config-dir) initial-load-path))


;; documentation
(with-eval-after-load 'info
  (add-to-list 'Info-additional-directory-list
               (ivan/emacs-file "info/")))

(with-eval-after-load 'help
  (setq source-directory "~/Development/code/elisp/emacs-mac"))


;; tramp
(setq tramp-default-method "ssh")


;; https
(setq tls-checktrust t)
(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  (setq
   tls-program (list (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                             (if (eq window-system 'w32) ".exe" "") trustfile))
   gnutls-verify-error t
   gnutls-trustfiles (list trustfile)))


;; packages
(require 'package)

(setq package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")
                         ("marmalade"    . "https://ojab.ru/marmalade/")))

(setq
 package--init-file-ensured t   ; don't add a 'package-initialize' call to my init file
 package-enable-at-startup  nil
 package-user-dir           ivan/packages-dir
 package-archive-priorities '(
                              ("melpa-stable" . 3)
                              ("gnu"          . 2)
                              ("marmalade"    . 1)
                              ("melpa"        . 0)
                              )
 )

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(setq use-package-always-ensure t)

(use-package doom-themes
  :pin melpa
  :config
  (setq
   doom-enable-bold   t
   doom-enable-italic t
   ivan/want-brighten-minibuffer nil
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

  (defun ivan/update-doom-settings ()
    (if (or (memq 'doom-one custom-enabled-themes)
            (memq 'elixir   custom-enabled-themes))
        (ivan/activate-doom-config)
      (ivan/deactivate-doom-config))
    (powerline-reset))

  (defun ivan/activate-doom-config ()
    (add-hook 'find-file-hook #'doom-buffer-mode)
    (when ivan/want-brighten-minibuffer
      (add-hook 'minibuffer-setup-hook #'doom-brighten-minibuffer))
    (mapc #'ivan/maybe-enable-doom-buffer-mode (buffer-list)))

  (defun ivan/maybe-enable-doom-buffer-mode (buffer)
    (with-current-buffer buffer
      (unless (string-match-p "^[ *]" (buffer-name))
        (doom-buffer-mode +1))))

  (defun ivan/deactivate-doom-config ()
    (remove-hook 'find-file-hook #'doom-buffer-mode)
    (remove-hook 'minibuffer-setup-hook #'doom-brighten-minibuffer)
    (doom-buffer-mode 0))

  (add-hook 'ivan/rotated-theme-hook #'ivan/update-doom-settings)
  (add-hook 'after-init-hook #'ivan/update-doom-settings)

  (when (display-graphic-p) (require 'doom-neotree)))

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

(use-package beacon
  :config
  (beacon-mode +1)
  (setq
   beacon-color (face-background 'region)
   beacon-blink-when-point-moves-vertically 10
   beacon-blink-delay    0.2
   beacon-blink-duration 0.25
   )
  )

(use-package f
  :commands (f-dirname f-relative))

(use-package all-the-icons
  :ensure t)

(use-package evil
  :pin melpa
  :demand
  :bind
  (:map evil-normal-state-map
        ("'"           . evil-use-register)
        ("U"           . undo-tree-redo)
        ("C-r"         . isearch-backward)
        ("S-SPC"       . ivan/emacs-state-rectangle-mark-mode)
        ("C-<return>"  . ivan/add-whitespace-below)
        ("S-<return>"  . ivan/add-whitespace-above)
        ("C-m"         . evil-next-line-first-non-blank)
        ("C-t"         . transpose-words)
        ("M-t"         . transpose-sexps)
        ("≠"           . evil-numbers/inc-at-pt)
        ("–"           . evil-numbers/dec-at-pt)
        ("t"           . ivan/run-tests-or-find-char-to)
        ("T"           . ivan/run-test-file-or-find-char-to-backward)
        ("S-SPC"       . evil-ex)
        :map evil-motion-state-map
        ("C-d"         . ivan/kill-buffer-and-maybe-window)
        ("C-e"         . evil-end-of-line)
        ("C-S-E"       . evil-scroll-line-down)
        ("C-w C-h"     . evil-window-left)
        ("C-w C-j"     . evil-window-down)
        ("C-w C-k"     . evil-window-up)
        ("C-w C-l"     . evil-window-right)
        ("C-w s"       . split-window-below)
        ("C-w C-s"     . split-window-below)
        ("C-w SPC"     . ivan/zoom-window)
        ("C-w C-SPC"   . ivan/zoom-window)
        ("C-w S-SPC"   . ivan/zoom-other-window)
        ("M-S-SPC"     . eval-expression)
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
        ("C-S-U"       . ivan/backward-kill-line)
        ("C-<return>"  . evil-open-below)
        ("S-<return>"  . evil-open-above)
        ("C-d"         . nil)
        ("C-t"         . nil)
        :map evil-replace-state-map
        ("M-v"         . yank)
        ("C-e"         . evil-copy-from-below)
        ("C-y"         . evil-copy-from-above)
        )
  :config
  (progn
    (add-hook 'after-init-hook #'evil-mode)
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
            (neotree-mode           . motion)
            (message-mode           . normal)
            (debugger-mode          . normal)
            (image-mode             . normal)
            (doc-view-mode          . normal)
            (eww-mode               . normal)
            (tabulated-list-mode    . normal)
            (view-mode              . normal)
            (profile-report-mode    . emacs)
            (comint-mode            . emacs)
            (cider-repl-mode        . emacs)
            (term-mode              . emacs)
            (calendar-mode          . emacs)
            (Man-mode               . emacs)))
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
    (add-hook 'prog-mode-hook #'ivan/treat-underscore-as-word-char)
    (dolist (hook '(emacs-lisp-mode-hook
                    clojure-mode-hook
                    scheme-mode-hook
                    lisp-mode-hook))
      (add-hook hook #'ivan/treat-hyphen-as-word-char))
    (defun ivan/treat-underscore-as-word-char () (ivan/treat-as-word-char ?_))
    (defun ivan/treat-hyphen-as-word-char     () (ivan/treat-as-word-char ?-))
    (defun ivan/treat-as-word-char (char) (modify-syntax-entry char "w"))

    (defun ivan/paste-pop-or-previous-line (count)
      (interactive "p")
      (if (memq last-command
                '(evil-paste-after
                  evil-paste-before
                  evil-visual-paste))
          (evil-paste-pop count)
        (evil-previous-line-first-non-blank count)))

    (defun ivan/paste-pop-or-next-line (count)
      (interactive "p")
      (ivan/paste-pop-or-previous-line (- count)))

    (define-key evil-normal-state-map "\C-n" #'ivan/paste-pop-or-next-line)
    (define-key evil-normal-state-map "\C-p" #'ivan/paste-pop-or-previous-line)

    (defun ivan/run-tests-or-find-char-to (count char)
      (interactive "p\nc")
      (if (= char ?\r)
          (ivan/run-test)
        (evil-find-char-to count char)))

    (defun ivan/run-test ()
      (cond
       ((eq 'ruby-mode major-mode)
        (ivan/rspec-dwim 'rspec-verify-single))
       ((eq 'emacs-lisp-mode major-mode)
        (ivan/ert))))

    (defun ivan/ert ()
      (let ((original-window (selected-window))
            (original-frame (selected-frame)))
        (ert-run-tests-interactively t)
        (select-frame-set-input-focus original-frame 'no-record)
        (select-window original-window 'no-record)))

    (defun ivan/run-test-file-or-find-char-to-backward (count char)
      (interactive "p\nc")
      (if (= char ?\r)
          (ivan/rspec-dwim 'rspec-verify)
        (evil-find-char-to-backward count char)))

    (defun ivan/emacs-state-rectangle-mark-mode ()
      (interactive)
      (evil-emacs-state)
      (rectangle-mark-mode +1))
    (defun ivan/backward-kill-line () (kill-line 0))
    (evil-define-key 'motion apropos-mode-map (kbd "<tab>") #'forward-button)
    (evil-define-key 'motion help-mode-map    (kbd "<tab>") #'forward-button)
    (evil-define-key 'motion help-mode-map    (kbd "C-o")   #'help-go-back)
    (evil-define-key 'motion help-mode-map    (kbd "C-i")   #'help-go-forward)
    (evil-define-key 'motion help-mode-map    (kbd "M-S-<return>") #'ivan/zoom-window-large)
    (evil-define-key 'motion apropos-mode-map (kbd "M-S-<return>") #'ivan/zoom-window-large)
    (evil-define-key 'motion neotree-mode-map (kbd "u") #'neotree-hidden-file-toggle)

    (setq evil-want-C-i-jump nil) ;; don't clobber TAB in terminal
    (define-key evil-motion-state-map [C-i] #'evil-jump-forward) ;; GUI only
    (define-key evil-normal-state-map (kbd "M-.") nil)
    (evil-define-key 'normal ggtags-mode-map (kbd "M-.") #'ggtags-find-tag-dwim)

    (defun ivan/move-key (keymap-from keymap-to key)
      "Moves key binding from one keymap to another, deleting from the old location."
      (define-key keymap-to key (lookup-key keymap-from key))
      (define-key keymap-from key nil))
    (ivan/move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))))

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

(use-package ggtags
  :commands (ggtags-mode
             ggtags-navigation-mode
             ggtags-find-tag-dwim)
  :diminish ggtags-mode
  :init
  (defvar ggtags-prog-modes '(ruby-mode)
    "Programming major modes in which ggtags is activated.")
  (defun ivan/maybe-enable-ggtags ()
    (when (apply 'derived-mode-p ggtags-prog-modes)
                (ggtags-mode +1)))
  (add-hook 'prog-mode-hook #'ivan/maybe-enable-ggtags)
  :config
  (defun ivan/add-ggtags-presenter ()
    (add-hook 'compilation-finish-functions #'ivan/present-search-results))
  (add-hook 'ggtags-global-mode-hook #'ivan/compilation-start-at-first-error)
  (add-hook 'ggtags-global-mode-hook #'ivan/add-ggtags-presenter))

(defun ivan/compilation-start-at-first-error ()
  (set (make-local-variable 'compilation-scroll-output) 'first-error))

(use-package rainbow-mode
  :diminish rainbow-mode
  :commands rainbow-mode
  :init
  (progn
    (defun ivan/colorize-theme ()
      (if (string-match "-theme.el$" (buffer-name)) (rainbow-mode +1)))
    (add-hook 'emacs-lisp-mode-hook #'ivan/colorize-theme))
  :config
  (add-hook 'rainbow-mode-hook (lambda () (hl-line-mode 0))))

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
          `(("." . ,(ivan/cache-file "undo-tree-history/"))))
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
  (defun ivan/maybe-reset-zoom (&rest _)
    (when (or (derived-mode-p 'help-mode)
              (derived-mode-p 'apropos-mode))
      (text-scale-set 0)
      (when (zoom-window--enable-p)
        (zoom-window-zoom))))

  (mapc (lambda (x) (advice-add x :before #'ivan/maybe-reset-zoom))
        '(quit-window
          evil-window-delete
          kill-buffer-and-window
          delete-other-windows))
  )

(defun ivan/zoom-window ()
  (interactive)
  (let ((window-side (window-parameter nil 'window-side)))
    (when window-side (set-window-parameter nil 'window-side nil))
    (zoom-window-zoom)
    (when window-side (set-window-parameter nil 'window-side window-side)))
  (powerline-reset))

(defun ivan/zoom-other-window ()
  (interactive)
  (if (zoom-window--enable-p)
      (progn
        (ivan/zoom-window)
        (other-window -1))
    (other-window 1)
    (ivan/zoom-window)))

(defun ivan/zoom-window-large ()
  (interactive)
  (ivan/zoom-window)
  (text-scale-set (if (zoom-window--enable-p) 1 0)))

(defun ivan/toggle-narrowing-zoom (p)
  (interactive "P")
  (ivan/zoom-window)
  (ivan/toggle-narrowing p))

(defun ivan/toggle-narrowing (p)
  (interactive "P")
  (cond
   ((and (buffer-narrowed-p) (not p))
    (widen))
   ((use-region-p)
    (narrow-to-region (region-beginning) (region-end)))
   (t
    (narrow-to-defun))))

(defun ivan/redraw-display ()
  (interactive)
  (redraw-display)
  (when (bound-and-true-p nlinum-mode)
    (nlinum--flush)))

(use-package smartparens
  :diminish smartparens-mode
  :init
  (add-hook 'prog-mode-hook #'smartparens-strict-mode)
  :config
  (require 'smartparens-config)
  (evil-define-key 'visual smartparens-mode-map
    "(" (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "(")))
  (evil-define-key 'visual smartparens-mode-map
    "[" (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "[")))
  (evil-define-key 'visual smartparens-mode-map
    "{" (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "{")))
  (evil-define-key 'visual smartparens-mode-map (kbd "C-9") #'sp-unwrap-sexp)
  (evil-define-key 'insert smartparens-mode-map (kbd "C-0") #'sp-forward-slurp-sexp)
  (evil-define-key 'insert smartparens-mode-map (kbd "C-9") #'sp-forward-barf-sexp)
  (evil-define-key 'insert smartparens-mode-map (kbd "M-9") #'sp-backward-slurp-sexp)
  (evil-define-key 'insert smartparens-mode-map (kbd "M-0") #'sp-backward-barf-sexp)
  (global-set-key (kbd "C-0") #'sp-forward-slurp-sexp)
  (global-set-key (kbd "C-9") #'sp-forward-barf-sexp)
  (global-set-key (kbd "M-9") #'sp-backward-slurp-sexp)
  (global-set-key (kbd "M-0") #'sp-backward-slurp-sexp)
  )

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
       (define-key evil-normal-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
       ;; Match selected region.
       (define-key evil-visual-state-map (kbd "M-d") 'evil-multiedit-match-and-next)

       ;; Same as M-d but in reverse.
       (define-key evil-normal-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)
       (define-key evil-visual-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)

       ;; OPTIONAL: If you prefer to grab symbols rather than words, use
       ;; `evil-multiedit-match-symbol-and-next` (or prev).

       ;; Restore the last group of multiedit regions.
       (define-key evil-visual-state-map (kbd "C-M-D") 'evil-multiedit-restore)

       ;; ...in visual mode, RET will disable all fields outside the selected region
       (define-key evil-visual-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

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
  (bind-map ivan/leader-map
    :evil-keys ("SPC")
    :evil-states (normal motion visual)
    :override-minor-modes t)
  (bind-map-set-keys ivan/leader-map
    ","          #'other-window
    "`"          #'variable-pitch-mode
    "="          #'align-regexp
    "SPC"        #'list-buffers
    "C-SPC"      #'shell-command
    "TAB"        #'neotree-toggle
    "q"          #'hydra-smartparens/body
    "C-l"        #'ivan/redraw-display
    "C-n"        #'ivan/toggle-narrowing
    ;; "C-r"        #'ripgrep-regexp
    "C-r"        #'evil-use-register
    "C-u"        #'hl-line-mode
    "X SPC"      #'server-edit
    "X s"        #'server-start
    "\\"         #'ivan/rotate-theme
    ;; "b SPC"    #'hydra-buffers/body
    "C-b"        #'bury-buffer
    "B"          #'unbury-buffer
    "b j"        #'bookmark-jump
    "b l"        #'bookmark-bmenu-list
    "b m"        #'bookmark-set
    "d d"        #'kill-this-buffer
    "f a"        #'find-alternate-file
    "f j"        #'dired-jump
    "f s"        #'save-buffer
    "f w"        #'write-file
    "i"          #'os-switch-to-term
    "a"          #'ag-project
    "C-a"        #'ag-project-regexp
    "g"          #'evil-goto-mark
    "l"          #'evil-switch-to-windows-last-buffer
    "m e b"      #'eval-buffer
    "m e f"      #'eval-defun
    "m e e"      #'pp-eval-last-sexp
    "m e r"      #'eval-region
    "m d"        #'mark-defun
    "o"          #'find-file
    "C-o"        #'switch-to-buffer
    "C-s"        search-map
    "C-v"        #'magit-blame
    "v s"        #'magit-status
    "v v"        #'vc-mode-line
    "w 0"        #'evil-window-delete
    "."          #'ivan/zoom-window
    "C-."        #'ivan/toggle-narrowing-zoom
    "w w"        #'ivan/zoom-other-window
    "w <return>" #'toggle-frame-fullscreen
    "w c"        #'evil-window-delete
    "w J"        #'webjump
    "w n"        #'ivan/toggle-narrowing
    "x"          #'execute-extended-command))

(use-package nlinum-relative
  :commands nlinum-relative-toggle
  :init
  (setq nlinum-relative-redisplay-delay 0.0)
  (setq-default nlinum-relative-current-symbol "0")
  (defvar-local ivan/line-numbers-p nil
    "Whether line-numbers should be displayed.")
  (defvar-local ivan/relative-line-numbers-p nil
    "Whether relative line-numbers should be displayed.")
  (use-package nlinum
    :init
    (add-hook 'prog-mode-hook #'ivan/init-line-numbers)
    (defun ivan/init-line-numbers ()
      (unless (string-match-p "^[ *]" (buffer-name))
        (ivan/toggle-line-numbers))))
  (defun ivan/toggle-line-numbers ()
    (interactive)
    (setq-local ivan/line-numbers-p (not ivan/line-numbers-p))
    (ivan//update-relative-line-numbers-style)
    (unless ivan/relative-line-numbers-p (ivan//update-line-numbers-display))
    )
  (defun ivan/toggle-relative-line-numbers ()
    (interactive)
    (setq-local ivan/relative-line-numbers-p (not ivan/relative-line-numbers-p))
    (ivan//update-relative-line-numbers-display)
    )
  (defun ivan//update-relative-line-numbers-style ()
    (setq nlinum-relative-current-symbol (if ivan/line-numbers-p "" "0"))
    )
  (defun ivan//update-line-numbers-display ()
    (nlinum-mode (if ivan/line-numbers-p +1 0))
  )
  (defun ivan//update-relative-line-numbers-display ()
    (if ivan/relative-line-numbers-p
        (ivan//relative-line-numbers-on)
      (ivan//relative-line-numbers-off))
    )
  (defun ivan//relative-line-numbers-on ()
    (nlinum-mode +1)
    (nlinum-relative-on)
    )
  (defun ivan//relative-line-numbers-off ()
    (nlinum-relative-off)
    (ivan//update-line-numbers-display)
    )
  (bind-map-set-keys ivan/leader-map
    "n l" #'ivan/toggle-line-numbers
    "n r" #'ivan/toggle-relative-line-numbers
    )
  )

(use-package indent-guide
  :commands indent-guide-mode
  :init
  (bind-map-set-keys ivan/leader-map
    "C-t i" #'indent-guide-mode
    )
  )

(use-package manage-minor-mode
  :commands manage-minor-mode
  :init
  (bind-map-set-keys ivan/leader-map
    "M" #'manage-minor-mode
    )
  :config
  (evil-define-key 'normal manage-minor-mode-map (kbd "q") #'quit-window))

(use-package evil-commentary
  :diminish evil-commentary-mode
  :init
  (progn
    (bind-map-set-keys ivan/leader-map
      ";" #'evil-commentary
      )
    (evil-commentary-mode +1)))

(use-package goto-chg
  :commands (goto-last-change
             goto-last-change-reverse))

(use-package ffap
  :commands ffap-other-window)

(use-package origami
  :commands ivan/toggle-origami-mode
  :init
  (defvar ivan/evil-fold-map
    (copy-keymap (lookup-key evil-normal-state-map "z"))
    "Reference to evil fold keybindings.")
  (bind-map-set-keys ivan/leader-map "z" #'ivan/toggle-origami-mode)
  :config
  (defun ivan/toggle-origami-mode ()
    (interactive)
    (if origami-mode
        (let ((map (copy-keymap ivan/evil-fold-map)))
          (origami-mode 0)
          (bind-key "z" map evil-normal-state-map))
      (origami-mode +1)
      (ivan/setup-origami-keybindings))
    )
  (defun ivan/setup-origami-keybindings ()
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

(use-package git-link
  :config
  (progn
    (defun ivan/open-git-link-in-browser (remote start end)
      (interactive (let* ((remote (if current-prefix-arg
                                      (git-link--read-remote)
                                    (git-link--remote)))
                          (region (git-link--get-region)))
                     (list remote (car region) (cadr region))))
      (let ((git-link-open-in-browser t))
        (git-link remote start end)))
    (bind-map-set-keys ivan/leader-map
      "v l" #'git-link
      "v L" #'ivan/open-git-link-in-browser)
    (defun ivan/git-link-github-handy (hostname dirname filename branch commit start end)
      (format "https://github.com/%s/blob/%s/%s#%s"
              dirname
              (or branch commit)
              filename
              (if end
                  (format "L%s-L%s" start end)
                (format "L%s" start))))
    (defun ivan/git-link-commit-github-handy (hostname dirname commit)
      (format "https://github.com/%s/commit/%s"
              dirname
              commit))
    (add-to-list 'git-link-remote-alist        '("github-handy" ivan/git-link-github-handy))
    (add-to-list 'git-link-commit-remote-alist '("github-handy" ivan/git-link-commit-github-handy))
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
    (bind-map-set-keys ivan/leader-map
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
      ("]"        ivan/increase-padding "increase")
      ("["        ivan/reduce-padding   "reduce")
      ("RET"      ivan/toggle-padding   "toggle")
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
  )

(use-package which-key
  :init (which-key-mode +1)
  :diminish which-key-mode
  :config
  (progn
    (which-key-declare-prefixes
      "SPC b"     "buffers/bookmarks"
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
    (mapc (lambda (x)
            (define-key drag-stuff-mode-map
              (kbd (car x)) (cdr x)))
          '(("C-M-k" . drag-stuff-up)
            ("C-M-j" . drag-stuff-down)
            ("C-M-l" . drag-stuff-right)
            ("C-M-h" . drag-stuff-left)))
    (drag-stuff-global-mode +1)
    )
  )

(use-package company
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        )
  )

(use-package swiper
  :commands ivy-mode)

(use-package counsel
  :commands counsel-ag
  :config
  (defun ivan/counsel-ag-project ()
    (interactive)
    (counsel-ag nil (locate-dominating-file default-directory ".git")))
  (bind-map-set-keys ivan/leader-map
    "C-r" #'ivan/counsel-ag-project))

(use-package ivy
  :commands ivy-read
  :diminish 'ivy-mode
  :bind
  (:map ivy-minibuffer-map
        ("<escape>" . minibuffer-keyboard-quit))
  :config
  (setq
   ivy-count-format ""
   ivy-format-function 'ivy-format-function-line
   ))

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
   )
  :config
  (progn
    (use-package dash)
    (use-package s)
    (setq
     ag-arguments (delete "--stats" ag-arguments)
     ag-highlight-search t)

    (define-key ag-mode-map
      [remap wgrep-change-to-wgrep-mode] #'ivan/init-wgrep-mode)

    (defun ivan/filter-ag-whitespace ()
      (ivan/filter-whitespace ag/file-column-pattern))
    (advice-add 'ag-filter :after #'ivan/filter-ag-whitespace)

    (add-hook 'ag-search-finished-hook #'ivan/present-search-results)
    (add-hook 'ag-mode-hook #'hl-line-mode)
    (add-hook 'ag-mode-hook #'ivan/compilation-start-at-first-error)))


(use-package magnet
  :ensure nil
  :load-path "lisp/magnet"
  :bind (:map evil-motion-state-map ("C-SPC" . magnet-toggle))
  :config
  (setq magnetic-modes '(ag-mode
                         apropos-mode
                         help-mode
                         rspec-compilation-mode
                         ert-results-mode
                         ggtags-global-mode
                         rake-compilation-mode)))
(use-package shackle
  :disabled t
  :config
  (shackle-mode +1)
  (setq
   shackle-rules '(;; Util
                   ("*eval*"          :align below :size 16  :noselect t)
                   ;; Emacs
                   ("*Apropos*"       :align below :size 0.3)
                   ("*Backtrace*"     :align below :size 25  :noselect t)
                   ("*Help*"          :align below :size 16  :select   t)
                   ("*Messages*"      :align below :size 15  :select   t)
                   ("*Warnings*"      :align below :size 10  :noselect t)
                   ("*Completions*"   :align below :size 0.2 :noselect t)
                   ;; Compilation
                   (compilation-mode  :align below :size 15  :noselect t)
                   ;; vcs
                   ("*vc-diff*"       :align below :size 15  :noselect t)
                   ("*vc-change-log*" :align below :size 15  :select   t)
                   (vc-annotate-mode  :same t)
                   ))
  )

(bind-map-for-mode-inherit ivan/compilation-leader-map ivan/leader-map
  :major-modes (compilation-mode)
  :bindings ("m f" #'next-error-follow-minor-mode))

(evil-define-key 'motion ert-results-mode-map (kbd "C-j") #'ert-results-next-test)
(evil-define-key 'motion ert-results-mode-map (kbd "C-k") #'ert-results-previous-test)
(evil-define-key 'motion compilation-mode-map (kbd "C-j") #'compilation-next-error)
(evil-define-key 'motion compilation-mode-map (kbd "C-k") #'compilation-previous-error)
(evil-define-key 'motion compilation-mode-map (kbd "M-n") #'make-frame)
(evil-define-key 'normal compilation-mode-map (kbd "M-n") #'make-frame)

(defun ivan/filter-whitespace (prefix-pattern)
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

(defun ivan/present-search-results (&rest _args)
  (ignore-errors
    (select-window (get-buffer-window (compilation-find-buffer)))
    (recenter 0)))

(defun ivan/without-side-splits (orig-fun &rest args)
  (let ((split-width-threshold nil))
    (apply orig-fun args)))

(advice-add 'compilation-start :around #'ivan/without-side-splits)

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
    (defun ivan/init-wgrep-mode ()
      (interactive)
      (advice-remove 'ag-filter #'ivan/filter-ag-whitespace)
      (add-hook 'ag-search-finished-hook #'ivan/enable-wgrep-mode)
      (recompile))
    (defun ivan/enable-wgrep-mode ()
      (remove-hook 'ag-search-finished-hook #'ivan/enable-wgrep-mode)
      (advice-add 'ag-filter :after #'ivan/filter-ag-whitespace)
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

(defun ivan/setup-org-mode ()
  (setq
   org-hide-leading-stars t
   line-spacing 0.15
   )
  (variable-pitch-mode +1))

(add-hook 'org-mode-hook #'ivan/setup-org-mode)

(use-package powerline
  :init
  (setq
   powerline-default-separator 'wave
   powerline-display-buffer-size nil
   powerline-display-mule-info   nil
   powerline-gui-use-vcs-glyph   t
   )
  )

(use-package core-modeline
  :ensure nil
  :load-path "lisp")

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

(use-package evil-magit
  :demand
  :init
  (progn
    (setq
     evil-magit-use-y-for-yank nil
     evil-magit-want-horizontal-movement t
     )
    )
  :config
  (progn
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
    ))

(use-package magit
  :config
  (progn
    (setq
     magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
     magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
     )))

(use-package flymake-ruby
  :diminish flymake-mode
  :init
  ;; (add-hook 'ruby-mode-hook 'flymake-ruby-load)
  )

(use-package rbenv
  :init
  (setq
   rbenv-show-active-ruby-in-modeline nil
   rbenv-executable (substring (shell-command-to-string "which rbenv") 0 -1)
   )
  (global-rbenv-mode +1)
  (add-hook 'ruby-mode-hook #'rbenv-use-corresponding))

(use-package projectile
  :commands projectile-find-file
  :config
  (setq projectile-completion-system 'ivy)
  (bind-key "M-O" 'projectile-find-file))

(use-package projectile-rails
  :diminish projectile-rails-mode
  :init
  (projectile-rails-global-mode +1)
  :config
  (setq rake-cache-file (ivan/cache-file "rake.cache")))

(use-package rspec-mode
  :pin melpa
  :diminish rspec-mode
  :commands (rspec-mode
             ivan/rspec-verify-dwim)
  :config
  (defun ivan/rspec-dwim (rspec-func)
    (interactive)
    (if (rspec-buffer-is-spec-p)
        (funcall rspec-func)
      (rspec-rerun)))

  (defun ivan/maybe-present-rspec-results (buffer _outcome)
    (when (eq 'rspec-compilation-mode major-mode)
      (ivan/present-rspec-results buffer)))

  (defun ivan/present-rspec-results (buffer)
    (let ((original-window (selected-window))
          (results-window  (get-buffer-window buffer 'visible)))
      (when results-window
        (select-window results-window)
        (next-line 3)
        (recenter-top-bottom 0)
        (select-window original-window))))

  (defun ivan/add-rspec-presenter ()
    (add-hook 'compilation-finish-functions #'ivan/maybe-present-rspec-results))
  (add-hook 'rspec-compilation-mode-hook #'ivan/add-rspec-presenter)
  (add-hook 'rspec-compilation-mode-hook #'ivan/wrap-lines))

(use-package cask-mode
  :mode "\\`Cask\\'")

(use-package yaml-mode
  :mode "\\.yml\\'"
  :config
  (add-hook 'yaml-mode-hook #'ivan/truncate-lines))

(use-package haml-mode :mode "\\.haml$")

(use-package exec-path-from-shell
  :defer t)

(use-package sh-script
  :mode ("/\\.?bash\\(/.*\\|rc\\|_profile\\|\\)$" . sh-mode)
  :init (add-hook 'sh-mode-hook (lambda () (setq mode-name "shell"))))

(use-package face-remap
  :diminish text-scale-mode
  :defer t)

(add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name "Elisp")))

;; gui & terminal
(defun ivan/text-scale-reset ()
  "Reset the height of the default face in the current buffer to its default value.
Disables `text-scale-mode`."
  (interactive)
  (text-scale-set 0))

(defun ivan/kill-buffer-and-maybe-window ()
  (interactive)
  (if (one-window-p)
      (kill-this-buffer)
    (kill-buffer-and-window)))

(defun ivan/delete-window ()
  (interactive)
  (if (one-window-p)
      (delete-frame)
    (delete-window)))

(defun ivan/global-text-scale-increase ()
  (interactive)
  (set-face-attribute
   'default nil :height (round (* (face-attribute 'default :height) 1.2))))

(defun ivan/global-text-scale-decrease ()
  (interactive)
  (set-face-attribute
   'default nil :height (round (/ (face-attribute 'default :height) 1.2))))

(defun configure-gui ()
  (bind-keys
   ("M-q"   . save-buffers-kill-terminal)
   ("M-A"   . mark-whole-buffer)
   ("M-o"   . find-file)
   ("M-c"   . kill-ring-save)
   ("M-w"   . ivan/delete-window)
   ("M-W"   . delete-frame)
   ("M-m"   . iconify-frame)
   ("M-n"   . make-frame)
   ("M-s"   . save-buffer)
   ("M-u"   . ivan/toggle-transparency)
   ("M-="   . text-scale-increase)
   ("M--"   . text-scale-decrease)
   ("M-)"   . ivan/text-scale-reset)
   ("M-+"   . ivan/global-text-scale-increase)
   ("M-_"   . ivan/global-text-scale-decrease)
   ("M-`"   . ns-next-frame)
   ("M-~"   . ns-prev-frame)
   ("M-<return>" . toggle-frame-fullscreen)
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
  (with-eval-after-load "evil"
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

  (defun ivan/adjust-terminal-colors ()
    (unless (display-graphic-p (selected-frame))
      (set-face-background 'default "white" (selected-frame))
      (set-face-background 'hl-line "#EEEEEE" (selected-frame)))
    )

  (add-hook 'window-setup-hook 'ivan/adjust-terminal-colors)
  )

(defconst IS-MAC (eq system-type 'darwin))

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
 ("C-p"  . isearch-ring-retreat)
 ("<up>" . isearch-ring-retreat)
 :map rectangle-mark-mode-map
 ("s" . string-rectangle)
 ("o" . rectangle-exchange-point-and-mark)
 :map shell-mode-map
 ("C-d" . comint-delchar-or-eof-or-kill-buffer)
 )

(global-set-key (kbd "C-w") #'ivan/kill-region-or-backward-kill-word)

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

(defun ivan/kill-completions-buffer ()
  (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer))))

(add-hook 'minibuffer-exit-hook #'ivan/kill-completions-buffer)

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

(defun ivan/isearch-exit ()
  "Run isearch-exit, and if in the minibuffer, submit the search result as input."
  (interactive)
  (isearch-exit)
  (when (minibuffer-window-active-p (selected-window))
    (let ((completion-fail-discreetly t))
      (minibuffer-complete-and-exit))))

;; hack through annoying minibuffer complaints
(defun ivan/command-error-function (data context caller)
  "Ignore the buffer-read-only signal; pass the rest to the default handler."
  (unless (eq 'buffer-read-only (car data))
    (command-error-default-function data context caller)))

(setq command-error-function #'ivan/command-error-function)

(defun ivan/minibuffer-C-w ()
  (interactive)
  (let ((fn (if (use-region-p) #'kill-region #'backward-kill-word)))
    (call-interactively fn)))

(add-hook 'minibuffer-setup-hook
          (lambda () (local-set-key (kbd "C-w") #'ivan/minibuffer-C-w)))


;; padding
(set-display-table-slot
 standard-display-table 0 ?\ )
(setq-default fringe-indicator-alist
              (assq-delete-all 'truncation fringe-indicator-alist))

(defvar ivan/padding-enabled nil)
(defvar ivan/padding-min 4)
(defvar ivan/padding-max 580)
(defvar ivan/padding-step 32)
(defvar ivan/padding-degree ivan/padding-min)

(add-to-list 'default-frame-alist `(left-fringe . ,ivan/padding-min))
(add-to-list 'default-frame-alist '(right-fringe . 1))

(defun ivan/config-overlay-arrow ()
  (define-fringe-bitmap 'left-bar [96 96 96 96 96 96 96 96 96 96 96 96])
  (defface left-bar-face
    `((t (:foreground ,(face-foreground 'mode-line))))
    "Face for `left-bar` bitmap.")
  (set-fringe-bitmap-face 'left-bar 'left-bar-face)
  (put 'overlay-arrow-position 'overlay-arrow-bitmap 'left-bar))
(add-hook 'ivan/rotated-theme-hook #'ivan/config-overlay-arrow)

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


;; etc.
(with-eval-after-load "isearch"
  (define-key isearch-mode-map (kbd "RET")        #'ivan/isearch-exit)
  (define-key isearch-mode-map (kbd "<return>")   #'ivan/isearch-exit)
  (define-key isearch-mode-map (kbd "S-<return>") #'isearch-exit))

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
    (newline)))

(defun ivan/minibuffer-setup-hook ()
  (setq gc-cons-threshold 339430400))

(defun ivan/minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'ivan/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'ivan/minibuffer-exit-hook)

(global-set-key [remap dabbrev-expand] #'hippie-expand)

(with-eval-after-load 'webjump
  (setq webjump-sites
        (append '(("stackoverflow" . "www.stackoverflow.com")
                  ("GitHub" . "https://github.com"))
                webjump-sample-sites)))

(add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

(defun ivan/goto-match-beginning ()
  (when (and isearch-forward isearch-other-end
             (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))
(add-hook 'isearch-mode-end-hook #'ivan/goto-match-beginning)

(add-hook 'ruby-mode-hook #'(lambda () (setq ruby-insert-encoding-magic-comment nil)))

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

(setq
 calendar-latitude 40.7
 calendar-longitude -74.0
 calendar-location-name "New York, NY"
 )
